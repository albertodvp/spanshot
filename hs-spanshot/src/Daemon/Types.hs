{-# LANGUAGE DeriveGeneric #-}

module Daemon.Types (
    DaemonState (..),
    WatchName,
    Watch (..),
    DaemonStats (..),
) where

import Data.Aeson (FromJSON (parseJSON), Options (..), ToJSON (toJSON), camelTo2, defaultOptions, genericParseJSON, genericToJSON)
import Data.Map.Strict (Map)
import Data.Sequence (Seq)
import Data.Text (Text)
import Data.Time (UTCTime)
import GHC.Generics (Generic)

import Config (Config)
import Types (SpanShot)

type WatchName = Text

data Watch
    = FileWatch !FilePath
    -- Future: | ProcessWatch ProcessHandle
    deriving (Show, Eq)

data DaemonState = DaemonState
    { dsConfig :: !Config
    , dsWatches :: !(Map WatchName Watch)
    , dsSpanShots :: !(Seq SpanShot)
    , dsStartTime :: !UTCTime
    }
    deriving (Show, Eq)

data DaemonStats = DaemonStats
    { statsUptime :: !Int -- seconds
    , statsWatchCount :: !Int
    , statsErrorCount :: !Int
    , statsPid :: !Int
    , statsConfigPath :: !FilePath
    }
    deriving (Show, Eq, Generic)

-- JSON serialization with snake_case (matches project convention)
instance ToJSON DaemonStats where
    toJSON =
        genericToJSON
            defaultOptions
                { fieldLabelModifier = camelTo2 '_' . drop 5 -- drop "stats" prefix
                }

instance FromJSON DaemonStats where
    parseJSON =
        genericParseJSON
            defaultOptions
                { fieldLabelModifier = camelTo2 '_' . drop 5
                }
