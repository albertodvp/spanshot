{-# LANGUAGE CPP #-}

module DaemonProcessSpec (daemonProcessTests) where

import System.Directory (doesDirectoryExist, doesFileExist)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec

#if !defined(mingw32_HOST_OS)
import Daemon.Process
#endif

daemonProcessTests :: Spec
daemonProcessTests = do
#if defined(mingw32_HOST_OS)
    describe "Daemon.Process (Windows)" $ do
        it "is not supported on Windows" $ do
            pendingWith "Daemon mode not yet supported on Windows"
#else
    describe "PID file management" $ do
        it "writes PID to file" $ do
            withSystemTempDirectory "spanshot-test" $ \dir -> do
                let pidFile = dir </> "test.pid"
                writePidFile pidFile 12345
                content <- readFile pidFile
                content `shouldBe` "12345"

        it "reads PID from file" $ do
            withSystemTempDirectory "spanshot-test" $ \dir -> do
                let pidFile = dir </> "test.pid"
                writeFile pidFile "12345"
                pid <- readPidFile pidFile
                pid `shouldBe` Just 12345

        it "returns Nothing for missing PID file" $ do
            withSystemTempDirectory "spanshot-test" $ \dir -> do
                let pidFile = dir </> "nonexistent.pid"
                pid <- readPidFile pidFile
                pid `shouldBe` Nothing

        it "returns Nothing for invalid PID file" $ do
            withSystemTempDirectory "spanshot-test" $ \dir -> do
                let pidFile = dir </> "test.pid"
                writeFile pidFile "not-a-number"
                pid <- readPidFile pidFile
                pid `shouldBe` Nothing

        it "removes PID file" $ do
            withSystemTempDirectory "spanshot-test" $ \dir -> do
                let pidFile = dir </> "test.pid"
                writeFile pidFile "12345"
                removePidFile pidFile
                exists <- doesFileExist pidFile
                exists `shouldBe` False

    describe "Daemon status check" $ do
        it "detects no daemon when PID file missing" $ do
            withSystemTempDirectory "spanshot-test" $ \dir -> do
                let pidFile = dir </> "test.pid"
                status <- getDaemonStatus pidFile
                status `shouldBe` DaemonNotRunning

        it "detects stale PID file (process not running)" $ do
            withSystemTempDirectory "spanshot-test" $ \dir -> do
                let pidFile = dir </> "test.pid"
                writeFile pidFile "999999999" -- Very unlikely to be running
                status <- getDaemonStatus pidFile
                status `shouldBe` DaemonStale

    describe "State directory" $ do
        it "creates state directory if missing" $ do
            withSystemTempDirectory "spanshot-test" $ \dir -> do
                let stateDir = dir </> "subdir" </> "spanshot"
                ensureStateDir stateDir
                exists <- doesDirectoryExist stateDir
                exists `shouldBe` True
#endif
