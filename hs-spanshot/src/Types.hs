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
    SpanShot (..),
    CaptureOptions (preWindowDuration, postWindowDuration, minContextEvents, detectionRules),
    mkCaptureOptions,
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

data CaptureOptions = CaptureOptions
    { preWindowDuration :: !NominalDiffTime
    , postWindowDuration :: !NominalDiffTime
    , minContextEvents :: !Int
    , detectionRules :: ![DetectionRule]
    }
    deriving (Show, Eq)

mkCaptureOptions ::
    NominalDiffTime ->
    NominalDiffTime ->
    Int ->
    [DetectionRule] ->
    Either String CaptureOptions
mkCaptureOptions preWin postWin minCtx rules
    | preWin < 0 = Left "preWindowDuration must be non-negative (>= 0 seconds)"
    | postWin < 0 = Left "postWindowDuration must be non-negative (>= 0 seconds)"
    | preWin == 0 && postWin == 0 = Left "At least one of preWindowDuration or postWindowDuration must be positive (> 0)"
    | minCtx < 1 = Left "minContextEvents must be at least 1"
    | null rules = Left "detectionRules cannot be empty"
    | otherwise =
        Right $
            CaptureOptions
                { preWindowDuration = preWin
                , postWindowDuration = postWin
                , minContextEvents = minCtx
                , detectionRules = rules
                }

defaultCaptureOptions :: CaptureOptions
defaultCaptureOptions =
    CaptureOptions
        { preWindowDuration = 5
        , postWindowDuration = 5
        , minContextEvents = 10
        , detectionRules = [RegexRule "ERROR"]
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
