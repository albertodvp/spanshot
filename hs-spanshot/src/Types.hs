{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use newtype instead of data" #-}

module Types (
    CollectEvent (..),
    CollectOptions (pollIntervalMs),
    mkCollectOptions,
    defaultCollectOptions,
    maxPollIntervalMs,
    minPollIntervalMs,
    DetectionRule (RegexRule, regexPattern),
    CompiledRule (..),
    SpanShot (..),
    CaptureOptions (preWindowDuration, postWindowDuration, minContextEvents, detectionRules, compiledRules, inactivityTimeout),
    mkCaptureOptions,
    mkCaptureOptionsWithTimeout,
    defaultCaptureOptions,
    ActiveCapture (..),
    CaptureState (..),
    initialCaptureState,
    spanShotToSeq,
    spanShotFromSeq,
) where

import Data.Aeson (FromJSON (parseJSON), Options (fieldLabelModifier), ToJSON (toJSON), camelTo2, defaultOptions, genericParseJSON, genericToJSON)
import Data.Foldable (toList)
import Data.Sequence (Seq)
import Data.Sequence qualified as Seq
import Data.Text (Text)
import Data.Time (NominalDiffTime, UTCTime)
import GHC.Generics (Generic)
import Text.Regex.TDFA (Regex, makeRegexM)
import Text.Regex.TDFA.String ()

data CollectEvent = CollectEvent
    { source :: !Text
    , sessionOrderId :: !Int
    , readAtUtc :: !UTCTime
    , line :: !Text
    }
    deriving (Show, Eq, Generic)

instance ToJSON CollectEvent where
    toJSON =
        genericToJSON
            defaultOptions
                { fieldLabelModifier = camelTo2 '_'
                }

instance FromJSON CollectEvent where
    parseJSON =
        genericParseJSON
            defaultOptions
                { fieldLabelModifier = camelTo2 '_'
                }

newtype CollectOptions = CollectOptions
    { pollIntervalMs :: Int
    }
    deriving (Show, Eq)

minPollIntervalMs :: Int
minPollIntervalMs = 10

maxPollIntervalMs :: Int
maxPollIntervalMs = 60000

mkCollectOptions :: Int -> Either String CollectOptions
mkCollectOptions interval
    | interval < minPollIntervalMs = Left $ "pollIntervalMs must be at least " <> show minPollIntervalMs <> " milliseconds"
    | interval > maxPollIntervalMs = Left $ "pollIntervalMs must be at most " <> show maxPollIntervalMs <> " milliseconds (1 minute)"
    | otherwise = Right $ CollectOptions{pollIntervalMs = interval}

defaultCollectOptions :: CollectOptions
defaultCollectOptions =
    CollectOptions
        { pollIntervalMs = 150
        }

data DetectionRule
    = RegexRule {regexPattern :: !String}
    deriving (Show, Eq, Generic)

instance ToJSON DetectionRule where
    toJSON =
        genericToJSON
            defaultOptions
                { fieldLabelModifier = camelTo2 '_'
                }

instance FromJSON DetectionRule where
    parseJSON =
        genericParseJSON
            defaultOptions
                { fieldLabelModifier = camelTo2 '_'
                }

{- | A compiled detection rule with pre-compiled regex for efficient matching.

This is used internally by 'CaptureOptions' to avoid re-compiling regex patterns
on every event. The 'crRule' field stores the original rule for serialization
and display, while 'crRegex' stores the compiled regex for matching.
-}
data CompiledRule = CompiledRule
    { crRule :: !DetectionRule
    , crRegex :: !Regex
    }

-- | Show instance that only shows the pattern (Regex has no Show instance)
instance Show CompiledRule where
    show (CompiledRule rule _) = "CompiledRule " ++ show rule

-- | Eq instance that compares only the original rule (Regex has no Eq instance)
instance Eq CompiledRule where
    (CompiledRule r1 _) == (CompiledRule r2 _) = r1 == r2

data SpanShot = SpanShot
    { errorEvent :: !CollectEvent
    , preWindow :: ![CollectEvent]
    , postWindow :: ![CollectEvent]
    , detectedBy :: ![DetectionRule]
    , capturedAtUtc :: !UTCTime
    }
    deriving (Show, Eq, Generic)

instance ToJSON SpanShot where
    toJSON =
        genericToJSON
            defaultOptions
                { fieldLabelModifier = camelTo2 '_'
                }

instance FromJSON SpanShot where
    parseJSON =
        genericParseJSON
            defaultOptions
                { fieldLabelModifier = camelTo2 '_'
                }

{- | Capture options with pre-compiled regex patterns.

The 'detectionRules' field stores the original rules for serialization,
while 'compiledRules' stores pre-compiled versions for efficient matching.

The 'inactivityTimeout' specifies how long to wait for new events before
flushing pending captures. This enables processing of static files where
all events are read instantly with similar timestamps.
-}
data CaptureOptions = CaptureOptions
    { preWindowDuration :: !NominalDiffTime
    , postWindowDuration :: !NominalDiffTime
    , minContextEvents :: !Int
    , detectionRules :: ![DetectionRule]
    , compiledRules :: ![CompiledRule]
    , inactivityTimeout :: !NominalDiffTime
    }
    deriving (Show, Eq)

{- | Create capture options with validation and regex pre-compilation.

This function validates all inputs and pre-compiles regex patterns for efficient
matching. If any regex pattern is invalid, returns Left with an error message.

The inactivityTimeout defaults to 2 * postWindowDuration, providing a buffer
to ensure post-windows complete before flushing.

Pre-compiling regexes is important for performance: without it, each event would
require re-compiling all regex patterns, which is expensive (O(m) per pattern
where m is the pattern length). With pre-compilation, matching is O(n) where n
is the input line length.
-}
mkCaptureOptions ::
    NominalDiffTime ->
    NominalDiffTime ->
    Int ->
    [DetectionRule] ->
    Either String CaptureOptions
mkCaptureOptions preWin postWin minCtx rules =
    -- Default timeout is 2 * postWindowDuration, but at least 1 second
    -- This handles the case where postWindowDuration is 0
    let defaultTimeout = max 1 (2 * postWin)
     in mkCaptureOptionsWithTimeout preWin postWin minCtx rules defaultTimeout

{- | Create capture options with explicit inactivity timeout.

Like 'mkCaptureOptions' but allows specifying a custom inactivity timeout
instead of using the default (2 * postWindowDuration).

The inactivity timeout must be at least as large as postWindowDuration
to ensure post-windows have a chance to complete normally.
-}
mkCaptureOptionsWithTimeout ::
    NominalDiffTime ->
    NominalDiffTime ->
    Int ->
    [DetectionRule] ->
    NominalDiffTime ->
    Either String CaptureOptions
mkCaptureOptionsWithTimeout preWin postWin minCtx rules timeout
    | preWin < 0 = Left "preWindowDuration must be non-negative (>= 0 seconds)"
    | postWin < 0 = Left "postWindowDuration must be non-negative (>= 0 seconds)"
    | preWin == 0 && postWin == 0 = Left "At least one of preWindowDuration or postWindowDuration must be positive (> 0)"
    | minCtx < 1 = Left "minContextEvents must be at least 1"
    | null rules = Left "detectionRules cannot be empty"
    | timeout <= 0 = Left "inactivityTimeout must be positive (> 0 seconds)"
    | postWin > 0 && timeout < postWin = Left "inactivityTimeout must be at least postWindowDuration"
    | otherwise =
        case compileDetectionRules rules of
            Left err -> Left err
            Right compiled ->
                Right $
                    CaptureOptions
                        { preWindowDuration = preWin
                        , postWindowDuration = postWin
                        , minContextEvents = minCtx
                        , detectionRules = rules
                        , compiledRules = compiled
                        , inactivityTimeout = timeout
                        }

{- | Compile all detection rules, returning an error if any regex is invalid.

This is a pure function that uses 'makeRegexM' which returns in 'MonadFail',
avoiding the need for 'unsafePerformIO'.
-}
compileDetectionRules :: [DetectionRule] -> Either String [CompiledRule]
compileDetectionRules = traverse compileRule
  where
    compileRule rule@(RegexRule pat) =
        case compileRegex pat of
            Nothing -> Left $ "Invalid regex pattern: " ++ pat
            Just regex -> Right $ CompiledRule rule regex

{- | Compile a regex pattern, returning Nothing if invalid.

Uses 'makeRegexM' with 'Maybe' as the MonadFail instance, providing a pure
way to attempt regex compilation without exceptions.
-}
compileRegex :: String -> Maybe Regex
compileRegex = makeRegexM

defaultCaptureOptions :: CaptureOptions
defaultCaptureOptions =
    let defaultRules = [RegexRule "ERROR"]
        defaultCompiled = case compileDetectionRules defaultRules of
            Right compiled -> compiled
            Left e -> error $ "defaultCaptureOptions: invalid default rule: " <> e
        defaultPostWindow = 5
     in CaptureOptions
            { preWindowDuration = 5
            , postWindowDuration = defaultPostWindow
            , minContextEvents = 10
            , detectionRules = defaultRules
            , compiledRules = defaultCompiled
            , inactivityTimeout = 2 * defaultPostWindow
            }

{- | Active capture state during post-window collection.

Memory note: 'acPostEvents' grows unbounded until 'postWindowDuration' elapses.
In high-throughput scenarios, this may consume significant memory proportional
to log volume within that time window. See 'processEvent' for details.
-}
data ActiveCapture = ActiveCapture
    { acErrorEvent :: !CollectEvent
    , acDetectedBy :: ![DetectionRule]
    , acPreWindowSnapshot :: !(Seq CollectEvent)
    , acPostEvents :: !(Seq CollectEvent)
    }
    deriving (Show, Eq)

data CaptureState = CaptureState
    { csPreWindow :: !(Seq CollectEvent)
    , csActiveCapture :: !(Maybe ActiveCapture)
    }
    deriving (Show, Eq)

initialCaptureState :: CaptureState
initialCaptureState =
    CaptureState
        { csPreWindow = Seq.empty
        , csActiveCapture = Nothing
        }

spanShotToSeq :: SpanShot -> (CollectEvent, Seq CollectEvent, Seq CollectEvent)
spanShotToSeq shot =
    ( errorEvent shot
    , Seq.fromList (preWindow shot)
    , Seq.fromList (postWindow shot)
    )

spanShotFromSeq :: CollectEvent -> Seq CollectEvent -> Seq CollectEvent -> [DetectionRule] -> UTCTime -> SpanShot
spanShotFromSeq err pre post rules time =
    SpanShot
        { errorEvent = err
        , preWindow = toList pre
        , postWindow = toList post
        , detectedBy = rules
        , capturedAtUtc = time
        }
