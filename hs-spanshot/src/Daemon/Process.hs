{-# LANGUAGE CPP #-}

module Daemon.Process (
    DaemonStatus (..),
    writePidFile,
    readPidFile,
    removePidFile,
    getDaemonStatus,
    ensureStateDir,
    getSocketPath,
    getPidPath,
#if !defined(mingw32_HOST_OS)
    daemonize,
    isProcessRunning,
#endif
) where

import Control.Exception (IOException, try)
import Control.Monad (when)
import System.Directory (
    XdgDirectory (XdgState),
    createDirectoryIfMissing,
    doesFileExist,
    getXdgDirectory,
    removeFile,
 )
import System.FilePath (takeDirectory, (</>))
import Text.Read (readMaybe)

#if !defined(mingw32_HOST_OS)
import System.Exit (ExitCode (ExitSuccess))
import System.Posix.IO (closeFd, stdError, stdInput, stdOutput)
import System.Posix.Process (createSession, exitImmediately, forkProcess)
import System.Posix.Signals (nullSignal, signalProcess)
#endif

data DaemonStatus
    = DaemonRunning Int
    | DaemonStale -- PID file exists but process not running
    | DaemonNotRunning
    deriving (Show, Eq)

-- | Get XDG state directory for spanshot
getStateDir :: IO FilePath
getStateDir = getXdgDirectory XdgState "spanshot"

getSocketPath :: IO FilePath
getSocketPath = (</> "spanshot.sock") <$> getStateDir

getPidPath :: IO FilePath
getPidPath = (</> "spanshot.pid") <$> getStateDir

ensureStateDir :: FilePath -> IO ()
ensureStateDir = createDirectoryIfMissing True

writePidFile :: FilePath -> Int -> IO ()
writePidFile path pid = do
    ensureStateDir (takeDirectory path)
    writeFile path (show pid)

readPidFile :: FilePath -> IO (Maybe Int)
readPidFile path = do
    result <- try (readFile path) :: IO (Either IOException String)
    pure $ case result of
        Left _ -> Nothing
        Right content -> readMaybe content

removePidFile :: FilePath -> IO ()
removePidFile path = do
    exists <- doesFileExist path
    when exists $ removeFile path

#if !defined(mingw32_HOST_OS)
isProcessRunning :: Int -> IO Bool
isProcessRunning pid = do
    result <- try (signalProcess nullSignal (fromIntegral pid)) :: IO (Either IOException ())
    pure $ case result of
        Left _ -> False
        Right _ -> True

getDaemonStatus :: FilePath -> IO DaemonStatus
getDaemonStatus pidPath = do
    mpid <- readPidFile pidPath
    case mpid of
        Nothing -> pure DaemonNotRunning
        Just pid -> do
            running <- isProcessRunning pid
            pure $
                if running
                    then DaemonRunning pid
                    else DaemonStale

-- | Daemonize the current process (Unix only)
daemonize :: IO () -> IO ()
daemonize action = do
    -- First fork
    _ <- forkProcess $ do
        -- Create new session
        _ <- createSession
        -- Second fork (prevent acquiring controlling terminal)
        _ <- forkProcess $ do
            -- Close standard file descriptors
            closeFd stdInput
            closeFd stdOutput
            closeFd stdError
            -- Run the daemon action
            action
        -- Exit first child
        exitImmediately ExitSuccess
    -- Parent continues
    pure ()

#else
getDaemonStatus :: FilePath -> IO DaemonStatus
getDaemonStatus pidPath = do
    mpid <- readPidFile pidPath
    case mpid of
        Nothing -> pure DaemonNotRunning
        Just _ -> pure DaemonStale -- Can't check on Windows easily
#endif
