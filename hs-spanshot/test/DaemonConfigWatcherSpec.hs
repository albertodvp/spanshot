module DaemonConfigWatcherSpec (daemonConfigWatcherTests) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.STM
import Data.Time (getCurrentTime)
import Data.Yaml qualified as Yaml
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec

import Config (CaptureConfig (..), Config (..), defaultConfig)
import Daemon.ConfigWatcher (startConfigWatcher)
import Daemon.State (config, initialState)
import Daemon.Types (DaemonState)

daemonConfigWatcherTests :: Spec
daemonConfigWatcherTests = do
    describe "Config file watcher" $ do
        it "detects config file changes" $ do
            withSystemTempDirectory "spanshot-test" $ \dir -> do
                let configPath = dir </> "config.yaml"
                now <- getCurrentTime
                stateVar <- newTVarIO (initialState now defaultConfig)

                -- Write initial config
                Yaml.encodeFile configPath defaultConfig

                -- Start watcher
                stopWatcher <- startConfigWatcher configPath stateVar

                -- Wait for watcher to start
                threadDelay 200000 -- 200ms

                -- Modify config
                let newCaptureConfig =
                        (capture defaultConfig)
                            { ccMinContextEvents = 99
                            }
                let newConfig = defaultConfig{capture = newCaptureConfig}
                Yaml.encodeFile configPath newConfig

                -- Wait for fsnotify to detect change
                threadDelay 500000 -- 500ms

                -- Check state updated
                state <- readTVarIO stateVar
                ccMinContextEvents (capture (config state)) `shouldBe` 99

                -- Cleanup
                stopWatcher

        it "handles missing config file gracefully" $ do
            withSystemTempDirectory "spanshot-test" $ \dir -> do
                let configPath = dir </> "nonexistent.yaml"
                now <- getCurrentTime
                stateVar <- newTVarIO (initialState now defaultConfig)

                -- Start watcher on non-existent file (should not crash)
                stopWatcher <- startConfigWatcher configPath stateVar

                threadDelay 100000 -- 100ms

                -- State should be unchanged
                state <- readTVarIO stateVar
                config state `shouldBe` defaultConfig

                stopWatcher
