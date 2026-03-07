{- | Wrap mode for single command monitoring.

This module provides the ability to run a single command with
SpanShot monitoring, preserving the exit code while capturing
any errors with context.
-}
module Wrap (
    -- * Wrap Operations
    runWrap,
    WrapResult (..),
) where

import Capture (captureFromStream)
import Config (capture, loadConfig, toCaptureOptions)
import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Data.ByteString qualified as BS
import Data.IORef (IORef, atomicModifyIORef', modifyIORef', newIORef, readIORef)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Time (getCurrentTime)
import Storage qualified
import Streaming.Prelude qualified as S
import System.Exit (ExitCode (..))
import System.IO (Handle, hClose, hFlush, hIsEOF, stderr, stdout)
import System.Process (CreateProcess (..), StdStream (..), createProcess, proc, waitForProcess)
import Types (CollectEvent (..), SpanShot (..), defaultCaptureOptions)

-- | Result of a wrapped command execution
data WrapResult = WrapResult
    { wrapExitCode :: !ExitCode
    -- ^ Exit code from the wrapped command
    , wrapCaptureCount :: !Int
    -- ^ Number of captures made during execution
    }
    deriving (Show, Eq)

{- | Run a command with SpanShot monitoring.

Runs the command, captures stdout/stderr, detects errors,
and saves SpanShots while forwarding output to the terminal.
-}
runWrap :: FilePath -> [String] -> IO WrapResult
runWrap cmd args = do
    -- Load config for capture options
    (config, _warnings) <- loadConfig
    let captureOpts = either (const defaultCaptureOptions) id $ toCaptureOptions (capture config)

    -- Create process with piped stdout/stderr
    let procSpec =
            (proc cmd args)
                { std_out = CreatePipe
                , std_err = CreatePipe
                , std_in = Inherit
                }

    (_, mStdout, mStderr, ph) <- createProcess procSpec

    -- Refs for collecting events and counting
    eventsRef <- newIORef ([] :: [CollectEvent])
    orderIdRef <- newIORef (0 :: Int)
    captureCountRef <- newIORef (0 :: Int)

    -- Collect events from both stdout and stderr
    case (mStdout, mStderr) of
        (Just hOut, Just hErr) -> do
            -- MVar to signal completion
            stdoutDone <- newEmptyMVar
            stderrDone <- newEmptyMVar

            -- Fork thread to read stdout
            _ <- forkIO $ do
                readAndCollect hOut stdout "stdout" orderIdRef eventsRef
                putMVar stdoutDone ()

            -- Fork thread to read stderr
            _ <- forkIO $ do
                readAndCollect hErr stderr "stderr" orderIdRef eventsRef
                putMVar stderrDone ()

            -- Wait for both to finish
            takeMVar stdoutDone
            takeMVar stderrDone

            -- Close handles
            hClose hOut
            hClose hErr
        _ -> pure ()

    -- Wait for the process to exit
    exitCode <- waitForProcess ph

    -- Process collected events through capture pipeline
    events <- reverse <$> readIORef eventsRef
    let eventStream = S.each events
        spanshots = captureFromStream captureOpts eventStream

    -- Save any SpanShots
    S.mapM_
        ( \shot -> do
            _ <- Storage.saveCapture shot
            modifyIORef' captureCountRef (+ 1)
        )
        spanshots

    captureCount <- readIORef captureCountRef

    pure
        WrapResult
            { wrapExitCode = exitCode
            , wrapCaptureCount = captureCount
            }

-- | Read from a handle line by line, forward to output, and collect events
readAndCollect :: Handle -> Handle -> T.Text -> IORef Int -> IORef [CollectEvent] -> IO ()
readAndCollect hIn hOut sourceName orderIdRef eventsRef = do
    let loop = do
            eof <- hIsEOF hIn
            if eof
                then pure ()
                else do
                    bs <- BS.hGetLine hIn
                    now <- getCurrentTime

                    -- Get next order ID atomically
                    orderId <- atomicModifyIORef' orderIdRef (\n -> (n + 1, n))

                    -- Create event
                    let event =
                            CollectEvent
                                { source = sourceName
                                , sessionOrderId = orderId
                                , readAtUtc = now
                                , line = TE.decodeUtf8Lenient bs
                                }

                    -- Add to events list
                    modifyIORef' eventsRef (event :)

                    -- Forward to terminal
                    BS.hPutStr hOut bs
                    BS.hPutStr hOut "\n"
                    hFlush hOut

                    -- Continue
                    loop
    loop
