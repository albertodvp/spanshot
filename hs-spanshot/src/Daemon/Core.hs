{-# LANGUAGE CPP #-}

module Daemon.Core (
    startDaemon,
    runDaemonForeground,
) where

#if !defined(mingw32_HOST_OS)
import Control.Concurrent.Async (async, cancel)
import Control.Concurrent.STM
import Control.Exception (bracket)
import Data.Time (getCurrentTime)
import System.Directory (removeFile, doesFileExist)
import System.Posix.Process (getProcessID)
import System.Posix.Signals (Handler (Catch), installHandler, sigINT, sigTERM)
import Control.Monad (when)

import Config (getConfigPath, loadConfig)
import Daemon.ConfigWatcher (startConfigWatcher)
import Daemon.Process
import Daemon.Server (runServer)
import Daemon.State (initialState)

-- | Start daemon in background
startDaemon :: IO ()
startDaemon = do
    pidPath <- getPidPath
    status <- getDaemonStatus pidPath

    case status of
        DaemonRunning pid -> do
            putStrLn $ "Daemon already running (pid " ++ show pid ++ ")"
        DaemonStale -> do
            putStrLn "Removing stale PID file..."
            removePidFile pidPath
            launchDaemon
        DaemonNotRunning ->
            launchDaemon
  where
    launchDaemon = do
        daemonize runDaemonForeground
        putStrLn "Daemon started"

-- | Run daemon in foreground (for debugging or after daemonize)
runDaemonForeground :: IO ()
runDaemonForeground = do
    -- Setup paths
    socketPath <- getSocketPath
    pidPath <- getPidPath
    configPath <- getConfigPath

    -- Write PID file
    pid <- getProcessID
    writePidFile pidPath (fromIntegral pid)

    -- Load config
    cfg <- loadConfig
    now <- getCurrentTime
    stateVar <- newTVarIO (initialState now cfg)

    -- Setup signal handlers for graceful shutdown
    shutdownVar <- newTVarIO False
    let handleShutdown = atomically $ writeTVar shutdownVar True
    _ <- installHandler sigTERM (Catch handleShutdown) Nothing
    _ <- installHandler sigINT (Catch handleShutdown) Nothing

    -- Start components
    bracket
        ( do
            stopConfigWatcher <- startConfigWatcher configPath stateVar
            serverThread <- async $ runServer socketPath stateVar
            pure (stopConfigWatcher, serverThread)
        )
        ( \(stopWatcher, serverThread) -> do
            stopWatcher
            cancel serverThread
            removePidFile pidPath
            removeIfExists socketPath
        )
        ( \_ -> do
            -- Wait for shutdown signal
            atomically $ readTVar shutdownVar >>= check
            putStrLn "Shutting down..."
        )

removeIfExists :: FilePath -> IO ()
removeIfExists path = do
    exists <- doesFileExist path
    when exists $ removeFile path
#else
-- Windows stubs
startDaemon :: IO ()
startDaemon = putStrLn "Daemon mode not yet supported on Windows"

runDaemonForeground :: IO ()
runDaemonForeground = putStrLn "Daemon mode not yet supported on Windows"
#endif
