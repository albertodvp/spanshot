module Capture (
    detectError,
    runAllDetectors,
    addToPreWindow,
    processEvent,
) where

import Data.Sequence (Seq)
import Data.Sequence qualified as Seq
import Data.Time (addUTCTime)
import Text.Regex.TDFA ((=~))
import Text.Regex.TDFA.Text ()

import Data.Maybe (isNothing)
import Types (
    ActiveCapture (ActiveCapture, acDetectedBy, acErrorEvent, acPostEvents, acPreWindowSnapshot),
    CaptureOptions (detectionRules, minContextEvents, postWindowDuration, preWindowDuration),
    CaptureState (CaptureState, csActiveCapture, csPreWindow),
    CollectEvent (CollectEvent, line, readAtUtc, sessionOrderId, source),
    DetectionRule (RegexRule, regexPattern),
    SpanShot (SpanShot, capturedAtUtc, detectedBy, errorEvent, postWindow, preWindow),
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
  (typical regex matching complexity)
Space Complexity: O(1) - no additional allocation beyond regex engine
-}
detectError :: DetectionRule -> CollectEvent -> Bool
detectError (RegexRule pat) event =
    line event =~ pat

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
  m = average pattern length
Space Complexity: O(k') where k' = number of matching rules (≤ k)

Design note: We use filter for simplicity. For large rule sets (100s+),
consider optimizing with:
- Compiled regex caching
- Parallel rule evaluation
- Short-circuit on first match (if only one match needed)
-}
runAllDetectors :: [DetectionRule] -> CollectEvent -> [DetectionRule]
runAllDetectors rules event = filter (`detectError` event) rules

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
4. Return new state and any completed SpanShots (empty for now - Phase 4 handles completion)

Key behavior:
- Error event is NOT included in its own pre-window snapshot
- Error event IS added to main pre-window for future errors to see
- Single active capture policy: if capture already active, ignore new errors (Phase 5)

Example:

@
-- No error, just add to pre-window
state = CaptureState [t=1, t=2] Nothing
event = t=3 "INFO"
result = (CaptureState [t=1, t=2, t=3] Nothing, [])

-- Error detected, snapshot and create capture
state = CaptureState [t=1, t=2] Nothing
event = t=3 "ERROR"
result = (CaptureState [t=1, t=2, t=3] (Just capture), [])
  where capture.preWindowSnapshot = [t=1, t=2]  -- no t=3!
@

Time Complexity: O(n) for pre-window cleanup + O(k) for rule checking
Space Complexity: O(n) for snapshot creation when error detected
-}
processEvent :: CaptureOptions -> CaptureState -> CollectEvent -> (CaptureState, [SpanShot])
processEvent opts state newEvent =
    let
        matchedRules = runAllDetectors (detectionRules opts) newEvent
        isError = not (null matchedRules)

        newCapture =
            if isError && isNothing (csActiveCapture state)
                then
                    Just $
                        ActiveCapture
                            { acErrorEvent = newEvent
                            , acDetectedBy = matchedRules
                            , acPreWindowSnapshot = csPreWindow state
                            , acPostEvents = Seq.empty
                            }
                else csActiveCapture state

        updatedPreWindow = addToPreWindow opts (csPreWindow state) newEvent

        newState =
            CaptureState
                { csPreWindow = updatedPreWindow
                , csActiveCapture = newCapture
                }
     in
        (newState, [])
