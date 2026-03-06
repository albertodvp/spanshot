{-# LANGUAGE CPP #-}

{- | PTY session management for interactive terminal monitoring.

This module provides the main entry point for PTY-based session mode,
where a developer can work in a monitored shell and have errors
captured automatically.

Unix-only: Windows returns "not supported" error.
-}
module Session (
    -- * Session Operations
    runSession,
    SessionResult (..),
) where

import System.Exit (ExitCode (..))

#if defined(mingw32_HOST_OS)

import System.IO (hPutStrLn, stderr)

-- | Result of a session
data SessionResult = SessionResult
    { sessionExitCode :: !ExitCode
    , sessionCaptureCount :: !Int
    }
    deriving (Show, Eq)

-- | Run a PTY session (Windows stub - not supported)
runSession :: IO SessionResult
runSession = do
    hPutStrLn stderr "Error: session mode is not supported on Windows"
    pure SessionResult
        { sessionExitCode = ExitFailure 1
        , sessionCaptureCount = 0
        }

#else

import Control.Concurrent (forkIO, killThread)
import Control.Exception (bracket, finally)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BS8
import Data.IORef (newIORef, readIORef)
import System.Environment (lookupEnv)
import System.IO (hFlush, hSetBuffering, stdin, stdout, BufferMode (..))
import System.Posix.IO (stdInput)
import System.Posix.Terminal (TerminalAttributes, getTerminalAttributes, setTerminalAttributes, withoutMode, TerminalMode (..), TerminalState (..))

import Session.Pty (PtyHandle, closePty, readPty, spawnPty, waitPty, writePty)
import Session.State (Session, newSession, sessionId)

-- | Result of a session
data SessionResult = SessionResult
    { sessionExitCode :: !ExitCode
    -- ^ Exit code from the shell
    , sessionCaptureCount :: !Int
    -- ^ Number of captures made during the session
    }
    deriving (Show, Eq)

{- | Run a PTY session.

Spawns the user's shell in a PTY and runs an interactive session.
All output is monitored for errors and captures are saved automatically.
-}
runSession :: IO SessionResult
runSession = do
    -- Get user's shell
    shellPath <- getShellPath

    -- Create session state
    session <- newSession shellPath

    -- Print welcome message
    printWelcome session

    -- Spawn the shell in a PTY
    result <- spawnPty shellPath []
    case result of
        Left err -> do
            putStrLn $ "Error: Failed to start session: " ++ err
            pure SessionResult
                { sessionExitCode = ExitFailure 1
                , sessionCaptureCount = 0
                }
        Right handle -> do
            -- Run the session
            sessionResult <- runSessionLoop session handle
            -- Print summary
            printSummary sessionResult
            pure sessionResult

-- | Get the user's shell from SHELL env var or default to /bin/sh
getShellPath :: IO FilePath
getShellPath = do
    mShell <- lookupEnv "SHELL"
    pure $ maybe "/bin/sh" id mShell

-- | Print welcome message
printWelcome :: Session -> IO ()
printWelcome session = do
    putStrLn "Welcome to SpanShot session."
    putStrLn $ "Session ID: " ++ show (sessionId session)
    putStrLn "Type 'exit' to end the session."
    putStrLn ""
    hFlush stdout

-- | Print summary when session ends
printSummary :: SessionResult -> IO ()
printSummary result = do
    putStrLn ""
    putStrLn $ "SpanShot session ended. " ++ show (sessionCaptureCount result) ++ " capture(s) saved."
    hFlush stdout

-- | Main session loop: handle bidirectional I/O between stdin/stdout and PTY
runSessionLoop :: Session -> PtyHandle -> IO SessionResult
runSessionLoop _session handle = do
    captureCountRef <- newIORef (0 :: Int)

    -- Set terminal to raw mode for proper PTY handling
    bracket
        setupRawMode
        restoreTerminal
        $ \_ -> do
            -- Fork thread to read from stdin and write to PTY
            stdinThread <- forkIO $ stdinToPty handle

            -- Read from PTY and write to stdout in main thread
            let loop = do
                    result <- readPty handle
                    case result of
                        Left _err -> pure ()
                        Right bs
                            | BS.null bs -> pure ()
                            | otherwise -> do
                                BS8.putStr bs
                                hFlush stdout
                                -- TODO: Feed to capture pipeline here
                                loop

            -- Run the PTY output loop, then cleanup
            loop `finally` killThread stdinThread

            -- Wait for the shell to exit
            exitCode <- waitPty handle
            closePty handle

            captureCount <- readIORef captureCountRef

            pure SessionResult
                { sessionExitCode = exitCode
                , sessionCaptureCount = captureCount
                }

-- | Read from stdin and write to PTY
stdinToPty :: PtyHandle -> IO ()
stdinToPty handle = do
    let loop = do
            bs <- BS.hGetSome stdin 1024
            if BS.null bs
                then pure ()
                else do
                    _ <- writePty handle bs
                    loop
    loop

-- | Set terminal to raw mode and return old attributes for restoration
setupRawMode :: IO TerminalAttributes
setupRawMode = do
    oldAttrs <- getTerminalAttributes stdInput
    let rawAttrs = withoutMode (withoutMode (withoutMode oldAttrs EnableEcho) ProcessInput) KeyboardInterrupts
    setTerminalAttributes stdInput rawAttrs Immediately
    hSetBuffering stdin NoBuffering
    hSetBuffering stdout NoBuffering
    pure oldAttrs

-- | Restore terminal attributes
restoreTerminal :: TerminalAttributes -> IO ()
restoreTerminal oldAttrs = do
    setTerminalAttributes stdInput oldAttrs Immediately
    hSetBuffering stdin LineBuffering
    hSetBuffering stdout LineBuffering

#endif
