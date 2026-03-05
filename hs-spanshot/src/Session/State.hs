{- | Session state management.

This module provides types and functions for tracking session state,
including session ID generation and capture tracking.
-}
module Session.State (
    -- * Session Type
    Session (..),
    SessionId,

    -- * Session Operations
    newSession,
    addCapture,
    getCaptures,
    endSession,
) where

import Data.Text (Text)
import Data.Time (UTCTime)

-- | Unique identifier for a session
type SessionId = Text

{- | A PTY-based monitored shell session.

Tracks the session ID, start time, shell path, and captures created
during this session.
-}
data Session = Session
    { sessionId :: !SessionId
    , sessionStartTime :: !UTCTime
    , sessionShellPath :: !FilePath
    , sessionCaptureIds :: ![Text]
    , sessionIsActive :: !Bool
    }
    deriving (Show, Eq)

-- | Create a new session with a unique ID
newSession :: FilePath -> IO Session
newSession _ = error "Session.State.newSession: not yet implemented"

-- | Add a capture ID to the session
addCapture :: Session -> Text -> Session
addCapture session captureId =
    session{sessionCaptureIds = captureId : sessionCaptureIds session}

-- | Get all capture IDs from the session
getCaptures :: Session -> [Text]
getCaptures = sessionCaptureIds

-- | Mark session as ended
endSession :: Session -> Session
endSession session = session{sessionIsActive = False}
