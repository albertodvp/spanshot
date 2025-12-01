module Daemon.ConfigWatcher (
    startConfigWatcher,
) where

import Control.Concurrent.STM
import Control.Exception (SomeException, try)
import System.FSNotify
import System.FilePath (takeDirectory, takeFileName)

import Config (loadConfigFrom)
import Daemon.State (updateConfig)
import Daemon.Types (DaemonState)

-- | Start watching config file for changes
-- Returns an action to stop the watcher
startConfigWatcher :: FilePath -> TVar DaemonState -> IO (IO ())
startConfigWatcher configPath stateVar = do
    mgr <- startManager

    let dir = takeDirectory configPath
    let fileName = takeFileName configPath

    _ <- watchDir mgr dir (isConfigFile fileName) $ \_event -> do
        -- Reload config on any change
        result <- try $ loadConfigFrom configPath
        case result of
            Left (err :: SomeException) ->
                -- Log error but don't crash
                putStrLn $ "Config reload failed: " ++ show err
            Right newConfig ->
                atomically $ modifyTVar' stateVar (updateConfig newConfig)

    pure $ stopManager mgr
  where
    isConfigFile name event =
        takeFileName (eventPath event) == name
