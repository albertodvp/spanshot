module Capture (
    detectError,
    runAllDetectors,
) where

import Data.Text qualified as T
import Text.Regex.TDFA ((=~))

import Types (CollectEvent (..), DetectionRule (..))

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
    T.unpack (line event) =~ pat

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
