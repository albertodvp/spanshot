{- | Capture storage with LRU eviction.

This module provides filesystem-based storage for SpanShot captures,
with automatic LRU eviction when the configured limit is exceeded.

Storage location: @.spanshot/captures/*.json@
-}
module Storage (
    -- * Storage Operations
    saveCapture,
    loadCapture,
    listCaptures,

    -- * LRU Eviction
    enforceLimit,

    -- * Paths
    getCapturesDir,
    getCaptureFilePath,
) where

import Data.Text (Text)

import Types (SpanShot)

-- | Get the captures directory path (.spanshot/captures/)
getCapturesDir :: IO FilePath
getCapturesDir = error "Storage.getCapturesDir: not yet implemented"

-- | Get the file path for a specific capture ID
getCaptureFilePath :: Text -> IO FilePath
getCaptureFilePath _ = error "Storage.getCaptureFilePath: not yet implemented"

-- | Save a capture to disk, returning the capture ID
saveCapture :: SpanShot -> IO Text
saveCapture _ = error "Storage.saveCapture: not yet implemented"

-- | Load a capture by ID
loadCapture :: Text -> IO (Either String SpanShot)
loadCapture _ = error "Storage.loadCapture: not yet implemented"

-- | List all capture IDs, most recent first
listCaptures :: IO [Text]
listCaptures = error "Storage.listCaptures: not yet implemented"

-- | Enforce the capture limit by deleting oldest captures (LRU eviction)
enforceLimit :: Int -> IO Int
enforceLimit _ = error "Storage.enforceLimit: not yet implemented"
