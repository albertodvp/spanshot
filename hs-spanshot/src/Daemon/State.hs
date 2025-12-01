module Daemon.State (
    -- * State construction
    initialState,
    -- * Watch management
    addWatch,
    removeWatch,
    listWatches,
    -- * SpanShot management
    addSpanShot,
    getSpanShot,
    listSpanShots,
    clearSpanShots,
    -- * Config management
    updateConfig,
    -- * Queries
    getUptime,
    -- * Accessor re-exports (cleaner names)
    config,
    watches,
    spanShots,
    startTime,
) where

import Data.Foldable (toList)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Sequence (Seq)
import Data.Sequence qualified as Seq
import Data.Time (UTCTime, diffUTCTime)

import Config (Config)
import Daemon.Types (DaemonState (..), Watch, WatchName)
import Types (SpanShot)

-- | Accessor for config (cleaner than dsConfig)
config :: DaemonState -> Config
config = dsConfig

-- | Accessor for watches
watches :: DaemonState -> Map WatchName Watch
watches = dsWatches

-- | Accessor for spanShots
spanShots :: DaemonState -> Seq SpanShot
spanShots = dsSpanShots

-- | Accessor for startTime
startTime :: DaemonState -> UTCTime
startTime = dsStartTime

-- | Create initial daemon state
initialState :: UTCTime -> Config -> DaemonState
initialState time cfg =
    DaemonState
        { dsConfig = cfg
        , dsWatches = Map.empty
        , dsSpanShots = Seq.empty
        , dsStartTime = time
        }

-- | Add or update a watch
addWatch :: WatchName -> Watch -> DaemonState -> DaemonState
addWatch name watch state =
    state{dsWatches = Map.insert name watch (dsWatches state)}

-- | Remove a watch by name
removeWatch :: WatchName -> DaemonState -> DaemonState
removeWatch name state =
    state{dsWatches = Map.delete name (dsWatches state)}

-- | List all watches
listWatches :: DaemonState -> [(WatchName, Watch)]
listWatches = Map.toList . dsWatches

-- | Add a captured SpanShot (appends to end)
addSpanShot :: SpanShot -> DaemonState -> DaemonState
addSpanShot shot state =
    state{dsSpanShots = dsSpanShots state Seq.|> shot}

-- | Get a SpanShot by index (0-based)
getSpanShot :: Int -> DaemonState -> Maybe SpanShot
getSpanShot idx state = Seq.lookup idx (dsSpanShots state)

-- | List SpanShots with limit
listSpanShots :: Int -> DaemonState -> [SpanShot]
listSpanShots limit state =
    toList $ Seq.take limit (dsSpanShots state)

-- | Clear all captured SpanShots
clearSpanShots :: DaemonState -> DaemonState
clearSpanShots state = state{dsSpanShots = Seq.empty}

-- | Update config
updateConfig :: Config -> DaemonState -> DaemonState
updateConfig cfg state = state{dsConfig = cfg}

-- | Get daemon uptime in seconds
getUptime :: UTCTime -> DaemonState -> Int
getUptime now state =
    round $ diffUTCTime now (dsStartTime state)
