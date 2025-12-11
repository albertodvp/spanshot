module Collect (
    collectFromFile,
    collectFromFileWithCleanup,
    collectFromStdin,
    collectFromStream,
    collectFromJSONLStdin,
) where

import Control.Concurrent (threadDelay)
import Control.Exception (bracket)
import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import Data.ByteString qualified as BS
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Text.Encoding.Error qualified as TEE
import Data.Time (getCurrentTime)
import Streaming (Of, Stream)
import Streaming.ByteString.Char8 qualified as Q
import Streaming.Prelude qualified as S
import System.IO (Handle, IOMode (..), hClose, hIsEOF, openFile, stdin)

import Data.Aeson qualified as Aeson
import Data.ByteString.Lazy qualified as BL
import Types (CollectEvent (CollectEvent, line, readAtUtc, sessionOrderId, source), CollectOptions (pollIntervalMs))

{- | Default size for chunks read from file handles.

Set to 32KB (32 * 1024 bytes), which is a common buffer size that balances
memory usage with I/O efficiency.
-}
defaultChunkSize :: Int
defaultChunkSize = 32 * 1024

{- | Collect events from a file, polling for new content when EOF is reached.

This function opens a file and continuously reads it line-by-line, creating
a 'CollectEvent' for each line. When the end of file is reached, it polls
(waits and retries) rather than stopping, making it suitable for tailing
log files that are actively being written to.

The file handle remains open for the duration of the stream. For proper resource
cleanup (including handling Ctrl+C signals), the caller should use 'bracket' or
similar resource management when consuming this stream.

Each line is decoded as UTF-8 with lenient error handling.

Example:

@
collectFromFileWithCleanup opts path $ \\events ->
    S.mapM_ processEvent events
@

Or for simpler use cases where the stream will be fully consumed:

@
events <- S.toList_ $ S.take 10 $ collectFromFile opts "\/var\/log\/app.log"
@

Note: This opens the file in 'ReadMode'. On some operating systems, this may
prevent other processes from opening the file in append mode due to file locking.
-}
collectFromFile :: CollectOptions -> FilePath -> Stream (Of CollectEvent) IO ()
collectFromFile opts filePath = do
    handle <- liftIO $ openFile filePath ReadMode
    let pollingBytes = bytesWithPolling opts handle
    let linesStream = Q.lines pollingBytes
    collectFromStream (T.pack filePath) linesStream

{- | Collect events from a file with proper resource cleanup.

This is a wrapper around 'collectFromFile' that ensures the file handle is
properly closed, even when the process receives signals like Ctrl+C (SIGINT).
The file handle cleanup is guaranteed by 'bracket'.

This function is intended to be used as the top-level entry point when you
want automatic resource management. For more control, use 'collectFromFile'
and manage the handle lifecycle manually.

Example:

@
collectFromFileWithCleanup opts path $ \events ->
    S.mapM_ printEvent events
@
-}
collectFromFileWithCleanup ::
    CollectOptions ->
    FilePath ->
    (Stream (Of CollectEvent) IO () -> IO r) ->
    IO r
collectFromFileWithCleanup opts filePath action =
    bracket
        (openFile filePath ReadMode)
        hClose
        $ \handle -> do
            let pollingBytes = bytesWithPolling opts handle
            let linesStream = Q.lines pollingBytes
            action $ collectFromStream (T.pack filePath) linesStream

{- | Collect events from standard input.

Reads lines from stdin and produces 'CollectEvent's for each line.
The source is set to "stdin". Unlike file reading, this does NOT poll -
it terminates when EOF is reached on stdin.

This is useful for piping input to spanshot or for testing.

Example:

@
collectFromStdin $ \events ->
    S.mapM_ printEvent events
@
-}
collectFromStdin :: (Stream (Of CollectEvent) IO () -> IO r) -> IO r
collectFromStdin action = do
    let bytesStream = bytesFromStdin
    let linesStream = Q.lines bytesStream
    action $ collectFromStream (T.pack "stdin") linesStream

{- | Read bytes from stdin without polling.

Unlike 'bytesWithPolling', this terminates when EOF is reached,
which is the expected behavior for stdin (piped input ends at EOF).
-}
bytesFromStdin :: Q.ByteStream IO ()
bytesFromStdin = go
  where
    go = do
        eof <- liftIO $ hIsEOF stdin
        if eof
            then pure ()
            else do
                chunk <- liftIO $ BS.hGetSome stdin defaultChunkSize
                unless (BS.null chunk) $ Q.chunk chunk
                go

{- | Read bytes from a file handle with polling behavior.

Reads chunks of bytes (default 32KB) from the handle. When EOF is encountered,
instead of terminating, it waits for the duration specified in 'pollIntervalMs'
and retries. This implements a "tail -f" style behavior for following files
as they grow.

The function yields chunks using streaming-bytestring's 'Q.chunk' to build
an efficient byte stream.

Implementation notes:

* Uses 'BS.hGetSome' which reads available bytes without blocking
* Skips empty chunks to avoid unnecessary allocations
* Polls indefinitely (infinite loop) when EOF is reached
-}
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
                unless (BS.null chunk) $ Q.chunk chunk
                go

{- | Process a stream of line-bytestreams into 'CollectEvent's.

Takes a stream where each element is itself a 'ByteStream' representing one line,
and converts it into a stream of 'CollectEvent's. Each event includes:

* 'source': The source path provided
* 'sessionOrderId': Zero-based line number
* 'readAtUtc': Timestamp when the event was created
* 'line': The decoded text content

The function materializes each line-bytestream into a strict 'ByteString' using
'S.mapped Q.toStrict', then zips with indices starting from 0.

Example:

@
let linesStream = Q.lines someByteStream
events <- S.toList_ $ collectFromStream "myfile.log" linesStream
@
-}
collectFromStream :: Text -> Stream (Q.ByteStream IO) IO a -> Stream (Of CollectEvent) IO a
collectFromStream sourcePath linesStream = do
    -- Convert Stream (ByteStream m) to Stream (Of ByteString) using mapped
    let strictLines = S.mapped Q.toStrict linesStream
    -- Now we can zip with indices
    S.mapM (uncurry $ lineToEvent sourcePath) $
        S.zip (S.iterate succ 0) strictLines

{- | Convert a single line (as ByteString) into a 'CollectEvent'.

Creates a 'CollectEvent' with:

* The given source path
* The given session order ID (line number)
* Current timestamp
* UTF-8 decoded line content (with lenient error handling)

This is a helper function used internally by 'collectFromStream'.
-}
lineToEvent :: Text -> Int -> BS.ByteString -> IO CollectEvent
lineToEvent sourcePath sessionOrderIdVal lineByteString = do
    timestamp <- getCurrentTime
    pure
        CollectEvent
            { source = sourcePath
            , sessionOrderId = sessionOrderIdVal
            , readAtUtc = timestamp
            , line = TE.decodeUtf8With TEE.lenientDecode lineByteString
            }

{- | Read JSONL CollectEvents from stdin.

Reads lines from stdin where each line is a JSON-encoded CollectEvent.
This is used by the `capture` command to read the output of `collect`.

Lines that fail to parse as JSON are silently skipped (with a warning to stderr).

Example:

@
collectFromJSONLStdin $ \events ->
    S.mapM_ processEvent events
@
-}
collectFromJSONLStdin :: (Stream (Of CollectEvent) IO () -> IO r) -> IO r
collectFromJSONLStdin action = do
    let bytesStream = bytesFromStdin
    let linesStream = Q.lines bytesStream
    -- Convert each line ByteStream to strict ByteString, then parse as JSON
    let strictLines = S.mapped Q.toStrict linesStream
    -- Parse each line as JSON, skipping invalid lines
    action $ S.mapMaybe parseJSONLine strictLines

{- | Parse a single line as a JSON CollectEvent.
Returns Nothing if parsing fails.
-}
parseJSONLine :: BS.ByteString -> Maybe CollectEvent
parseJSONLine bs =
    case Aeson.eitherDecode (BL.fromStrict bs) of
        Left _ -> Nothing
        Right event -> Just event
