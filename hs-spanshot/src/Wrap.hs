{-# LANGUAGE CPP #-}

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

import System.Exit (ExitCode (..))

#if defined(mingw32_HOST_OS)

import System.IO (hPutStrLn, stderr)

-- | Result of a wrapped command execution
data WrapResult = WrapResult
    { wrapExitCode :: !ExitCode
    , wrapCaptureCount :: !Int
    }
    deriving (Show, Eq)

-- | Run a command with SpanShot monitoring (Windows stub)
runWrap :: FilePath -> [String] -> IO WrapResult
runWrap _ _ = do
    hPutStrLn stderr "Error: wrap mode is not supported on Windows"
    pure WrapResult
        { wrapExitCode = ExitFailure 1
        , wrapCaptureCount = 0
        }

#else

import Control.Exception (bracket)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BS8
import Data.IORef (newIORef, readIORef)
import System.IO (hFlush, stdout)

import Session.Pty (PtyHandle, closePty, readPty, spawnPty, waitPty)

-- | Result of a wrapped command execution
data WrapResult = WrapResult
    { wrapExitCode :: !ExitCode
    -- ^ Exit code from the wrapped command
    , wrapCaptureCount :: !Int
    -- ^ Number of captures made during execution
    }
    deriving (Show, Eq)

{- | Run a command with SpanShot monitoring.

Spawns the command in a PTY, forwards output to stdout,
and returns the exit code from the wrapped command.
-}
runWrap :: FilePath -> [String] -> IO WrapResult
runWrap cmd args = do
    result <- spawnPty cmd args
    case result of
        Left _err -> do
            -- Failed to spawn - return error
            pure WrapResult
                { wrapExitCode = ExitFailure 1
                , wrapCaptureCount = 0
                }
        Right handle ->
            bracket
                (pure handle)
                closePty
                (runWrapLoop)

-- | Main loop for wrap mode: read PTY output, forward to stdout, and wait for exit
runWrapLoop :: PtyHandle -> IO WrapResult
runWrapLoop handle = do
    captureCountRef <- newIORef (0 :: Int)

    -- Read and forward output until the process exits
    let loop = do
            result <- readPty handle
            case result of
                Left _err -> do
                    -- Read error, process probably exited
                    pure ()
                Right bs
                    | BS.null bs -> do
                        -- EOF, process exited
                        pure ()
                    | otherwise -> do
                        -- Forward output to stdout
                        BS8.putStr bs
                        hFlush stdout

                        -- TODO: Integrate with capture pipeline here
                        -- For now, just forward the output

                        -- Continue reading
                        loop

    loop

    -- Wait for the process to exit
    exitCode <- waitPty handle
    captureCount <- readIORef captureCountRef

    pure WrapResult
        { wrapExitCode = exitCode
        , wrapCaptureCount = captureCount
        }

#endif
