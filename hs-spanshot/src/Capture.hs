module Capture (
    detectError,
    detectErrorCompiled,
    runAllDetectors,
    runAllDetectorsCompiled,
    addToPreWindow,
    processEvent,
    captureFromStream,
) where

import Data.Foldable (toList)
import Data.Sequence (Seq)
import Data.Sequence qualified as Seq
import Data.Text qualified as T
import Data.Time (addUTCTime, diffUTCTime)
import Text.Regex.TDFA (matchTest, (=~))
import Streaming (Of, Stream, lift)
import Streaming.Prelude qualified as S
import Text.Regex.TDFA ((=~))
import Text.Regex.TDFA.Text ()

import Types (
    ActiveCapture (ActiveCapture, acDetectedBy, acErrorEvent, acPostEvents, acPreWindowSnapshot),
    CaptureOptions (compiledRules, minContextEvents, postWindowDuration, preWindowDuration),
    CaptureState (CaptureState, csActiveCapture, csPreWindow),
    CollectEvent (line, readAtUtc),
    CompiledRule (..),
    DetectionRule (RegexRule),
    SpanShot (SpanShot, capturedAtUtc, detectedBy, errorEvent, postWindow, preWindow),
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
- Post-window is bounded by postWindowDuration (time) only, with no hard event count limit
- In high-throughput scenarios (e.g., 1000s of events/second), post-window memory scales
  with log volume within postWindowDuration. This is a deliberate simplification for v0.1;
  a maxPostWindowEvents option may be added in future versions if needed.
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
                    shouldEmit = elapsed >= postWindowDuration opts
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

{- | Transform a stream of CollectEvents into a stream of SpanShots.

This is the main entry point for streaming capture. It maintains internal
state and emits SpanShots when errors are detected and post-windows complete.

The function processes events one by one, threading state through the stream,
and emitting a SpanShot only when a post-window duration has elapsed after
an error detection.

Note: When the input stream ends, any active capture that hasn't completed
its post-window will be dropped. This is a deliberate simplification for v0.1.

Example:

@
let events = collectFromFile opts "app.log"
let captureOpts = defaultCaptureOptions { detectionRules = [RegexRule "ERROR"] }
S.mapM_ printSpanShot $ captureFromStream captureOpts events
@

Time Complexity: O(1) per event (delegates to processEvent)
Space Complexity: O(n) where n = max(preWindow size, postWindow size)
-}
captureFromStream ::
    (Monad m) =>
    CaptureOptions ->
    Stream (Of CollectEvent) m r ->
    Stream (Of SpanShot) m r
captureFromStream opts = go initialCaptureState
  where
    go state stream = do
        result <- lift $ S.next stream
        case result of
            Left r -> pure r
            Right (event, rest) ->
                let (newState, maybeShot) = processEvent opts state event
                 in case maybeShot of
                        Nothing -> go newState rest
                        Just shot -> do
                            S.yield shot
                            go newState rest
