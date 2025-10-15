module Collect
  ( collectFromFile
  , collectFromStream
  , CollectOptions (..)
  ) where

import Control.Concurrent (threadDelay)
import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString as BS
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Encoding.Error as TEE
import Data.Time (getCurrentTime)
import Streaming (Of, Stream)
import qualified Streaming.ByteString.Char8 as Q
import qualified Streaming.Prelude as S
import System.IO (Handle, IOMode (..), hIsEOF, openFile)

import Types (CollectOptions (..), CollectEvent (..))

-- | Collect events from a file, polling for new content when EOF is reached.
--
-- This function opens a file and continuously reads it line-by-line, creating
-- a 'CollectEvent' for each line. When the end of file is reached, it polls
-- (waits and retries) rather than stopping, making it suitable for tailing
-- log files that are actively being written to.
--
-- The file handle is never closed, as this is designed for infinite streaming.
-- Each line is decoded as UTF-8 with lenient error handling.
--
-- Example:
--
-- @
-- events <- S.toList_ $ S.take 10 $ collectFromFile opts "\/var\/log\/app.log"
-- @
--
-- Note: This opens the file in 'ReadMode'. On some operating systems, this may
-- prevent other processes from opening the file in append mode due to file locking.
collectFromFile :: CollectOptions -> FilePath -> Stream (Of CollectEvent) IO ()
collectFromFile opts filePath = do
  handle <- liftIO $ openFile filePath ReadMode
  let pollingBytes = bytesWithPolling opts handle
  let linesStream = Q.lines pollingBytes
  collectFromStream (T.pack filePath) linesStream

-- | Read bytes from a file handle with polling behavior.
--
-- Reads chunks of bytes (default 32KB) from the handle. When EOF is encountered,
-- instead of terminating, it waits for the duration specified in 'pollIntervalMs'
-- and retries. This implements a "tail -f" style behavior for following files
-- as they grow.
--
-- The function yields chunks using streaming-bytestring's 'Q.chunk' to build
-- an efficient byte stream.
--
-- Implementation notes:
--
-- * Uses 'BS.hGetSome' which reads available bytes without blocking
-- * Skips empty chunks to avoid unnecessary allocations
-- * Polls indefinitely (infinite loop) when EOF is reached
bytesWithPolling :: CollectOptions -> Handle -> Q.ByteStream IO ()
bytesWithPolling opts handle = go
  where
    go = do
      eof <- liftIO $ hIsEOF handle
      if eof
        then do
          liftIO $ threadDelay (pollIntervalMs opts * 1000)
          go
        else do
          chunk <- liftIO $ BS.hGetSome handle defaultChunkSize
          unless (BS.null chunk) $ do
            Q.chunk chunk
            go
    
    defaultChunkSize = 32 * 1024

-- | Process a stream of line-bytestreams into 'CollectEvent's.
--
-- Takes a stream where each element is itself a 'ByteStream' representing one line,
-- and converts it into a stream of 'CollectEvent's. Each event includes:
--
-- * 'source': The source path provided
-- * 'sessionOrderId': Zero-based line number
-- * 'readAtUtc': Timestamp when the event was created
-- * 'line': The decoded text content
--
-- The function materializes each line-bytestream into a strict 'ByteString' using
-- 'S.mapped Q.toStrict', then zips with indices starting from 0.
--
-- Example:
--
-- @
-- let linesStream = Q.lines someByteStream
-- events <- S.toList_ $ collectFromStream "myfile.log" linesStream
-- @
collectFromStream :: Text -> Stream (Q.ByteStream IO) IO () -> Stream (Of CollectEvent) IO ()
collectFromStream sourcePath linesStream = do
  -- Convert Stream (ByteStream m) to Stream (Of ByteString) using mapped
  let strictLines = S.mapped Q.toStrict linesStream
  -- Now we can zip with indices
  S.mapM (uncurry $ lineToEvent sourcePath)
    $ S.zip (S.each [0..]) strictLines

-- | Convert a single line (as ByteString) into a 'CollectEvent'.
--
-- Creates a 'CollectEvent' with:
--
-- * The given source path
-- * The given session order ID (line number)
-- * Current timestamp
-- * UTF-8 decoded line content (with lenient error handling)
--
-- This is a helper function used internally by 'collectFromStream'.
lineToEvent :: Text -> Int -> BS.ByteString -> IO CollectEvent
lineToEvent sourcePath sessionOrderId lineByteString = do
  timestamp <- getCurrentTime
  pure
    CollectEvent
      { source = sourcePath
      , sessionOrderId = sessionOrderId
      , readAtUtc = timestamp
      , line = TE.decodeUtf8With TEE.lenientDecode lineByteString
      }
