module Capture (
    detectError,
    detectErrorCompiled,
    runAllDetectors,
    runAllDetectorsCompiled,
    addToPreWindow,
    processEvent,
    finalizeCapture,
    captureFromStream,
    captureFromStreamWithTicks,
    checkTimeout,
) where

import Data.Foldable (toList)
import Data.Sequence (Seq)
import Data.Sequence qualified as Seq
import Data.Text qualified as T
import Data.Time (UTCTime, addUTCTime, diffUTCTime, getCurrentTime)
import Streaming (Of ((:>)), Stream)
import Streaming qualified as S
import Streaming.Internal (Stream (Effect, Return, Step))
import System.Timeout (timeout)
import Text.Regex.TDFA (matchTest, (=~))
import Text.Regex.TDFA.Text ()

import Types (
    ActiveCapture (ActiveCapture, acDetectedBy, acErrorEvent, acPostEvents, acPreWindowSnapshot),
    CaptureOptions (compiledRules, maxPostWindowEvents, minContextEvents, postWindowDuration, preWindowDuration),
    CaptureState (CaptureState, csActiveCapture, csPreWindow),
    CollectEvent (line, readAtUtc),
    CompiledRule (..),
    DetectionRule (RegexRule),
    SpanShot (SpanShot, captureId, captureSource, capturedAtUtc, detectedBy, errorEvent, postWindow, preWindow, sessionId, truncated),
    initialCaptureState,
 )

{- | Check if a detection rule matches a collect event.

Takes a 'DetectionRule' and applies it to a 'CollectEvent', returning
'True' if the event matches the rule, 'False' otherwise.

Currently supports:
* 'RegexRule': Matches if the event's line matches the regex pattern

Example:

@
let rule = RegexRule "ERROR"
event <- createEvent "2025-10-14 ERROR Failed"
detectError rule event  -- Returns True
@

Time Complexity: O(n × m) where n = line length, m = pattern length
  (typical regex matching complexity, includes regex compilation)
Space Complexity: O(1) - no additional allocation beyond regex engine

Note: This function re-compiles the regex on each call. For better performance
in hot paths, use 'detectErrorCompiled' with pre-compiled rules.
-}
detectError :: DetectionRule -> CollectEvent -> Bool
detectError (RegexRule pat) event =
    line event =~ pat

{- | Check if a pre-compiled rule matches a collect event.

This is the high-performance version of 'detectError' that uses a pre-compiled
regex pattern. Use this in hot paths (e.g., when processing log streams) to
avoid the overhead of regex compilation on every event.

Time Complexity: O(n) where n = line length (matching only, no compilation)
Space Complexity: O(1)
-}
detectErrorCompiled :: CompiledRule -> CollectEvent -> Bool
detectErrorCompiled (CompiledRule _ regex) event =
    matchTest regex (T.unpack $ line event)

{- | Run all detection rules against an event.

Returns a list of all rules that matched the event. If no rules match,
returns an empty list. The order of rules in the result matches the
order in the input list.

Example:

@
let rules = [RegexRule "ERROR", RegexRule "FATAL"]
event <- createEvent "FATAL ERROR occurred"
runAllDetectors rules event  -- Returns [RegexRule "ERROR", RegexRule "FATAL"]
@

Time Complexity: O(k × n × m) where:
  k = number of rules
  n = line length
  m = average pattern length (includes regex compilation per rule)
Space Complexity: O(k') where k' = number of matching rules (≤ k)

Note: This function re-compiles regexes on each call. For better performance
in hot paths, use 'runAllDetectorsCompiled' with pre-compiled rules.
-}
runAllDetectors :: [DetectionRule] -> CollectEvent -> [DetectionRule]
runAllDetectors rules event = filter (`detectError` event) rules

{- | Run all pre-compiled detection rules against an event.

This is the high-performance version of 'runAllDetectors' that uses pre-compiled
regex patterns from 'CaptureOptions.compiledRules'.

Returns the original 'DetectionRule' for each match (for serialization).

Time Complexity: O(k × n) where:
  k = number of rules
  n = line length (matching only, no compilation)
Space Complexity: O(k') where k' = number of matching rules (≤ k)
-}
runAllDetectorsCompiled :: [CompiledRule] -> CollectEvent -> [DetectionRule]
runAllDetectorsCompiled rules event =
    [crRule cr | cr <- rules, detectErrorCompiled cr event]

{- | Add an event to the pre-window buffer with smart cleanup.

This function implements a two-phase cleanup strategy:
1. Time-based: Drop events older than preWindowDuration
2. Count-based fallback: If time-based cleanup leaves fewer than minContextEvents,
   keep the last N events instead

This ensures we always have sufficient context for error diagnosis, even in
sparse logging scenarios.

Example:

@
-- Dense logging (time-based cleanup works)
buffer = [t=5, t=6, t=7, t=8]
newEvent = t=10
preWindowDuration = 5s
minContextEvents = 2
Result: [t=6, t=7, t=8, t=10]  -- Cutoff at t=5

-- Sparse logging (fallback to count-based)
buffer = [t=0, t=1, t=2]
newEvent = t=100
preWindowDuration = 5s
minContextEvents = 3
Result: [t=1, t=2, t=100]  -- Keep last 3
@

Time Complexity: O(n) for dropWhileL + O(n) for drop = O(n)
Space Complexity: O(1) - structural sharing in Seq
-}
addToPreWindow :: CaptureOptions -> Seq CollectEvent -> CollectEvent -> Seq CollectEvent
addToPreWindow opts buffer newEvent =
    let
        withNew = buffer Seq.|> newEvent
        cutoff = addUTCTime (negate $ preWindowDuration opts) (readAtUtc newEvent)
        timeFiltered = Seq.dropWhileL (\e -> readAtUtc e < cutoff) withNew
        timeFilteredLen = Seq.length timeFiltered
        minEvents = minContextEvents opts
        cleaned =
            if timeFilteredLen >= minEvents
                then timeFiltered
                else
                    let totalLen = Seq.length withNew
                        dropCount = max 0 (totalLen - minEvents)
                     in Seq.drop dropCount withNew
     in
        cleaned

{- | Process a single event through the capture state machine.

This is the core of the streaming capture logic. For each incoming event:

1. Check if it matches any detection rules
2. If error detected:
   - Clone current pre-window as snapshot (excluding the error itself)
   - Create ActiveCapture with empty post-window
3. Update pre-window with the new event (including errors for future context)
4. Return new state and any completed SpanShots (emitted when postWindowDuration elapses)

Key behavior:
- Error event is NOT included in its own pre-window snapshot
- Error event IS added to main pre-window for future errors to see
- Single active capture policy: if capture already active, ignore new errors
- Post-window events are accumulated until postWindowDuration elapses, then SpanShot is emitted

Example:

@
-- No error, just add to pre-window
state = CaptureState [t=1, t=2] Nothing
event = t=3 "INFO"
result = (CaptureState [t=1, t=2, t=3] Nothing, Nothing)

-- Error detected, snapshot and create capture
state = CaptureState [t=1, t=2] Nothing
event = t=3 "ERROR"
result = (CaptureState [t=1, t=2, t=3] (Just capture), Nothing)
  where capture.preWindowSnapshot = [t=1, t=2]  -- no t=3!

-- Post-window completes, emit SpanShot
state = CaptureState [t=1, t=2, t=3] (Just activeCapture)
event = t=8 "INFO"  -- 5 seconds after error
result = (CaptureState [t=1, t=2, t=3, t=8] Nothing, Just spanshot)
@

Time Complexity: O(n) for pre-window cleanup + O(k) for rule checking
Space Complexity: O(n) for snapshot creation when error detected

Memory considerations:
- Pre-window is bounded by preWindowDuration (time) and minContextEvents (count fallback)
- Post-window is bounded by BOTH postWindowDuration (time) AND maxPostWindowEvents (count)
- When maxPostWindowEvents is reached before time expires, SpanShot is emitted with truncated=True
- Default maxPostWindowEvents is 1000, preventing memory exhaustion in high-throughput scenarios
-}
processEvent :: CaptureOptions -> CaptureState -> CollectEvent -> (CaptureState, Maybe SpanShot)
processEvent opts state newEvent =
    let
        -- Use compiled rules for efficient matching (no re-compilation per event)
        matchedRules = runAllDetectorsCompiled (compiledRules opts) newEvent
        isError = not (null matchedRules)
        updatedPreWindow = addToPreWindow opts (csPreWindow state) newEvent

        (newCapture, emittedShots) = case csActiveCapture state of
            Nothing
                | isError ->
                    ( Just $
                        ActiveCapture
                            { acErrorEvent = newEvent
                            , acDetectedBy = matchedRules
                            , acPreWindowSnapshot = csPreWindow state
                            , acPostEvents = Seq.empty
                            }
                    , Nothing
                    )
                | otherwise ->
                    (Nothing, Nothing)
            Just cap ->
                let
                    errorTime = readAtUtc (acErrorEvent cap)
                    eventTime = readAtUtc newEvent
                    elapsed = diffUTCTime eventTime errorTime
                    timeExpired = elapsed >= postWindowDuration opts
                    currentPostCount = Seq.length (acPostEvents cap)
                    -- Check if adding this event would exceed the limit
                    wouldExceedLimit = currentPostCount >= maxPostWindowEvents opts
                    shouldEmit = timeExpired || wouldExceedLimit
                    wasTruncated = wouldExceedLimit && not timeExpired
                 in
                    if shouldEmit
                        then
                            ( Nothing
                            , Just
                                SpanShot
                                    { errorEvent = acErrorEvent cap
                                    , preWindow = toList (acPreWindowSnapshot cap)
                                    , postWindow = toList (acPostEvents cap)
                                    , detectedBy = acDetectedBy cap
                                    , capturedAtUtc = eventTime
                                    , truncated = wasTruncated
                                    , captureId = Nothing
                                    , sessionId = Nothing
                                    , captureSource = Nothing
                                    }
                            )
                        else
                            let updatedPost = acPostEvents cap Seq.|> newEvent
                             in (Just cap{acPostEvents = updatedPost}, Nothing)

        newState =
            CaptureState
                { csPreWindow = updatedPreWindow
                , csActiveCapture = newCapture
                }
     in
        (newState, emittedShots)

{- | Convert an in-flight capture to a SpanShot when stream ends early.

This function is called during stream finalization when the input stream ends
while a capture is still in progress (post-window not yet complete). It creates
a SpanShot with whatever post-window events have been collected so far.

Example:

@
-- Error at t=10, stream ends at t=12 (before 5s post-window completes)
let cap = ActiveCapture errorEvt rules preSnap postEvts
let shot = finalizeCapture cap currentTime  -- Emits partial SpanShot
@

Time Complexity: O(n) where n = number of events in pre/post windows
Space Complexity: O(n) for the resulting SpanShot
-}
finalizeCapture :: ActiveCapture -> UTCTime -> SpanShot
finalizeCapture cap currentTime =
    SpanShot
        { errorEvent = acErrorEvent cap
        , preWindow = toList (acPreWindowSnapshot cap)
        , postWindow = toList (acPostEvents cap)
        , detectedBy = acDetectedBy cap
        , capturedAtUtc = currentTime
        , truncated = False -- Stream ended naturally, not due to event limit
        , captureId = Nothing
        , sessionId = Nothing
        , captureSource = Nothing
        }

{- | Transform a stream of CollectEvents into a stream of SpanShots.

This is the main streaming combinator for the capture phase. It processes each
incoming event through the pure 'processEvent' function, maintaining capture
state across events and emitting SpanShots when post-windows complete.

Key behaviors:

1. Maintains pre-window buffer (rolling window of recent events)
2. Detects errors using compiled regex rules from CaptureOptions
3. Captures pre-window snapshot when error detected
4. Collects post-window events until duration threshold reached
5. Emits SpanShot when post-window completes
6. On stream end, emits any in-flight capture (partial post-window)

The implementation uses direct pattern matching on Stream constructors
('Return', 'Effect', 'Step') which is the canonical pattern for stateful
stream transformers in the streaming library.

Example:

@
let opts = defaultCaptureOptions
let events = collectFromFile "app.log"
let spanshots = captureFromStream opts events
S.mapM_ (putStrLn . encode) spanshots
@

Memory considerations:
- State is strict (bang pattern) to prevent space leaks
- Pre-window bounded by 'preWindowDuration' and 'minContextEvents'
- Post-window bounded by 'postWindowDuration'

Time Complexity: O(1) per event (amortized), plus O(n) for finalization
Space Complexity: O(w) where w = max(pre-window events, post-window events)
-}
captureFromStream ::
    (Monad m) =>
    CaptureOptions ->
    Stream (Of CollectEvent) m r ->
    Stream (Of SpanShot) m r
captureFromStream opts = loop initialCaptureState
  where
    loop !state stream = case stream of
        -- FINALIZATION: Stream ended - emit in-flight capture if any
        Return r ->
            case csActiveCapture state of
                Nothing -> Return r
                Just cap ->
                    -- Use the last event's timestamp, or error event's timestamp if no post events
                    let finalTime = case Seq.viewr (acPostEvents cap) of
                            Seq.EmptyR -> readAtUtc (acErrorEvent cap)
                            _ Seq.:> lastEvt -> readAtUtc lastEvt
                     in Step (finalizeCapture cap finalTime :> Return r)
        -- EFFECTS: Preserve monadic layers
        Effect m -> Effect (fmap (loop state) m)
        -- DATA: Process event through existing pure processEvent
        Step (event :> rest) ->
            let (newState, maybeShot) = processEvent opts state event
             in case maybeShot of
                    Nothing -> loop newState rest
                    Just shot -> Step (shot :> loop newState rest)

{- | Check if the active capture's post-window has timed out.

This pure function checks if enough time has elapsed since the error was detected
to complete the post-window collection. If so, it returns the SpanShot and resets
the capture state.

This is used by 'captureFromStreamWithTicks' to emit SpanShots even when no new
events arrive (solving the "stuck SpanShot" problem).

Time Complexity: O(n) where n = number of events in windows
Space Complexity: O(n) for the resulting SpanShot
-}
checkTimeout :: CaptureOptions -> UTCTime -> CaptureState -> (CaptureState, Maybe SpanShot)
checkTimeout opts now state =
    case csActiveCapture state of
        Nothing -> (state, Nothing)
        Just cap ->
            let errorTime = readAtUtc (acErrorEvent cap)
                elapsed = diffUTCTime now errorTime
             in if elapsed >= postWindowDuration opts
                    then
                        let shot =
                                SpanShot
                                    { errorEvent = acErrorEvent cap
                                    , preWindow = toList (acPreWindowSnapshot cap)
                                    , postWindow = toList (acPostEvents cap)
                                    , detectedBy = acDetectedBy cap
                                    , capturedAtUtc = now
                                    , truncated = False
                                    , captureId = Nothing
                                    , sessionId = Nothing
                                    , captureSource = Nothing
                                    }
                            newState = state{csActiveCapture = Nothing}
                         in (newState, Just shot)
                    else (state, Nothing)

{- | Transform a stream of CollectEvents into a stream of SpanShots with timeout support.

This is an enhanced version of 'captureFromStream' that periodically checks for
timed-out post-windows even when no new events arrive. This solves the "stuck SpanShot"
problem where an error that causes the system to stop logging would never be emitted.

The tick interval determines how often to check for timeouts when no events arrive.
A typical value is 1 second (1000000 microseconds).

Key behaviors (in addition to 'captureFromStream'):
- Uses 'System.Timeout.timeout' to periodically check for expired post-windows
- Emits SpanShots when post-window duration elapses, even without new events
- On stream end, emits any in-flight capture

Example:

@
let opts = defaultCaptureOptions
let events = collectFromFileTail "app.log"
let tickInterval = 1000000  -- 1 second
let spanshots = captureFromStreamWithTicks opts tickInterval events
S.mapM_ (putStrLn . encode) spanshots
@

Memory considerations: Same as 'captureFromStream'
Time Complexity: O(1) per event or tick (amortized)

Note: This function is constrained to IO because 'System.Timeout.timeout' requires IO.
-}
captureFromStreamWithTicks ::
    CaptureOptions ->
    -- | Tick interval in microseconds (e.g., 1000000 = 1 second)
    Int ->
    Stream (Of CollectEvent) IO r ->
    Stream (Of SpanShot) IO r
captureFromStreamWithTicks opts tickMicros = loop initialCaptureState
  where
    loop !state stream = Effect $ do
        -- Try to get next event with timeout
        result <- timeout tickMicros (inspectStream stream)
        case result of
            Nothing -> do
                -- Tick: no event within timeout, check for expired post-window
                now <- getCurrentTime
                let (state', maybeShot) = checkTimeout opts now state
                pure $ case maybeShot of
                    Nothing -> loop state' stream
                    Just shot -> Step (shot :> loop state' stream)
            Just (Left r) -> do
                -- Stream ended, emit in-flight capture if any
                now <- getCurrentTime
                pure $ case csActiveCapture state of
                    Nothing -> Return r
                    Just cap -> Step (finalizeCapture cap now :> Return r)
            Just (Right (event, rest)) -> do
                -- Process the event normally
                let (newState, maybeShot) = processEvent opts state event
                pure $ case maybeShot of
                    Nothing -> loop newState rest
                    Just shot -> Step (shot :> loop newState rest)

    -- Inspect the stream to get the next element or end marker
    inspectStream :: Stream (Of CollectEvent) IO r -> IO (Either r (CollectEvent, Stream (Of CollectEvent) IO r))
    inspectStream s = do
        result <- S.inspect s
        case result of
            Left r -> pure (Left r)
            Right (event :> rest) -> pure (Right (event, rest))
