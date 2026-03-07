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
    listCapturesWithInfo,
    listCapturesBySession,
    getCaptureByIndex,

    -- * Capture Info
    CaptureInfo (..),

    -- * LRU Eviction
    enforceLimit,

    -- * Paths
    getCapturesDir,
    getCaptureFilePath,
) where

import Control.Exception (try)
import Data.Aeson qualified as Aeson
import Data.ByteString.Lazy qualified as LBS
import Data.List (sortOn)
import Data.Ord (Down (..))
import Data.Text (Text)
import Data.Text qualified as T
import Data.UUID qualified as UUID
import Data.UUID.V4 qualified as UUID
import System.Directory (
    createDirectoryIfMissing,
    doesFileExist,
    getModificationTime,
    listDirectory,
    removeFile,
 )
import System.FilePath (dropExtension, takeExtension, (</>))

import Data.Time (UTCTime)
import Types (CollectEvent (..), SpanShot (..))

-- | Get the captures directory path (.spanshot/captures/)
getCapturesDir :: IO FilePath
getCapturesDir = do
    let dir = ".spanshot" </> "captures"
    createDirectoryIfMissing True dir
    pure dir

-- | Get the file path for a specific capture ID
getCaptureFilePath :: Text -> IO FilePath
getCaptureFilePath captureId' = do
    dir <- getCapturesDir
    pure $ dir </> T.unpack captureId' <> ".json"

-- | Generate a unique capture ID using UUID v4
generateCaptureId :: IO Text
generateCaptureId = do
    uuid <- UUID.nextRandom
    pure $ T.pack $ UUID.toString uuid

-- | Save a capture to disk, returning the capture ID
saveCapture :: SpanShot -> IO Text
saveCapture shot = do
    captureId' <- generateCaptureId
    filePath <- getCaptureFilePath captureId'
    LBS.writeFile filePath (Aeson.encode shot)
    pure captureId'

-- | Load a capture by ID
loadCapture :: Text -> IO (Either String SpanShot)
loadCapture captureId' = do
    filePath <- getCaptureFilePath captureId'
    exists <- doesFileExist filePath
    if not exists
        then pure $ Left $ "Capture not found: " <> T.unpack captureId'
        else do
            result <- try $ LBS.readFile filePath
            case result of
                Left (e :: IOError) -> pure $ Left $ "Failed to read capture: " <> show e
                Right contents ->
                    case Aeson.eitherDecode contents of
                        Left err -> pure $ Left $ "Failed to parse capture JSON: " <> err
                        Right shot -> pure $ Right shot

-- | List all capture IDs, most recent first (by file modification time)
listCaptures :: IO [Text]
listCaptures = do
    dir <- getCapturesDir
    files <- listDirectory dir
    let jsonFiles = filter (\f -> takeExtension f == ".json") files

    -- Get modification times for sorting
    filesWithTimes <- mapM (getFileWithTime dir) jsonFiles

    -- Sort by modification time, most recent first
    let sorted = sortOn (Down . snd) filesWithTimes
        captureIds = map (T.pack . dropExtension . fst) sorted

    pure captureIds
  where
    getFileWithTime dir file = do
        time <- getModificationTime (dir </> file)
        pure (file, time)

{- | Enforce the capture limit by deleting oldest captures (LRU eviction)
Returns the number of captures deleted
-}
enforceLimit :: Int -> IO Int
enforceLimit limit = do
    captures <- listCaptures
    let count = length captures
        toDelete = max 0 (count - limit)

    if toDelete == 0
        then pure 0
        else do
            -- Delete the oldest captures (at the end of the list since it's sorted most recent first)
            let oldestCaptures = drop limit captures
            mapM_ deleteCapture oldestCaptures
            pure toDelete

-- | Delete a capture by ID
deleteCapture :: Text -> IO ()
deleteCapture captureId' = do
    filePath <- getCaptureFilePath captureId'
    exists <- doesFileExist filePath
    if exists
        then removeFile filePath
        else pure ()

-- | Information about a capture for display purposes
data CaptureInfo = CaptureInfo
    { ciCapturedAt :: !UTCTime
    -- ^ When the capture was made
    , ciSessionId :: !(Maybe Text)
    -- ^ Session ID if captured in a session
    , ciTriggerLine :: !Text
    -- ^ First line of the error event
    }
    deriving (Show, Eq)

-- | List captures with their info, most recent first
listCapturesWithInfo :: IO [(Text, CaptureInfo)]
listCapturesWithInfo = do
    captureIds <- listCaptures
    capturesWithInfo <- mapM loadCaptureInfo captureIds
    pure [(cid, info) | (cid, Just info) <- capturesWithInfo]
  where
    loadCaptureInfo :: Text -> IO (Text, Maybe CaptureInfo)
    loadCaptureInfo cid = do
        result <- loadCapture cid
        case result of
            Left _ -> pure (cid, Nothing)
            Right shot -> pure (cid, Just (extractInfo shot))

    extractInfo :: SpanShot -> CaptureInfo
    extractInfo shot =
        CaptureInfo
            { ciCapturedAt = capturedAtUtc shot
            , ciSessionId = sessionId shot
            , ciTriggerLine = line (errorEvent shot)
            }

-- | List captures filtered by session ID, most recent first
listCapturesBySession :: Maybe Text -> IO [Text]
listCapturesBySession Nothing = listCaptures
listCapturesBySession (Just sid) = do
    infos <- listCapturesWithInfo
    pure [cid | (cid, info) <- infos, ciSessionId info == Just sid]

-- | Get a capture by its 1-based index (1 = most recent)
getCaptureByIndex :: Int -> IO (Either String (Text, SpanShot))
getCaptureByIndex idx
    | idx <= 0 = pure $ Left "Index must be a positive number"
    | otherwise = do
        captures <- listCaptures
        if idx > length captures
            then pure $ Left $ "Index " ++ show idx ++ " is out of range (only " ++ show (length captures) ++ " captures)"
            else do
                let cid = captures !! (idx - 1)
                result <- loadCapture cid
                case result of
                    Left err -> pure $ Left err
                    Right shot -> pure $ Right (cid, shot)
