{-# LANGUAGE CPP #-}

module DaemonServerSpec (daemonServerTests) where

#if !defined(mingw32_HOST_OS)
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (async, cancel)
import Control.Concurrent.STM
import Data.ByteString.Char8 qualified as BS
import Data.Time (getCurrentTime)
import Network.Socket
import Network.Socket.ByteString (recv, sendAll)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
#endif

import Test.Hspec

#if !defined(mingw32_HOST_OS)
import Config (defaultConfig)
import Daemon.Server (runServer)
import Daemon.State (initialState)
import Daemon.Types (DaemonState)
#endif

daemonServerTests :: Spec
daemonServerTests = do
#if defined(mingw32_HOST_OS)
    describe "Daemon.Server (Windows)" $ do
        it "is not supported on Windows" $ do
            pendingWith "Daemon mode not yet supported on Windows"
#else
    describe "Unix socket server" $ do
        around withTestServer $ do
            it "accepts connection and responds to status" $ \(socketPath, _) -> do
                response <- sendTestRequest socketPath "{\"method\": \"status\"}"
                response `shouldContain` "\"ok\":true"

            it "handles watch.add command" $ \(socketPath, _) -> do
                response <-
                    sendTestRequest
                        socketPath
                        "{\"method\": \"watch.add\", \"params\": {\"name\": \"test\", \"path\": \"./test.log\"}}"
                response `shouldContain` "\"ok\":true"

            it "handles watch.list after add" $ \(socketPath, _) -> do
                _ <-
                    sendTestRequest
                        socketPath
                        "{\"method\": \"watch.add\", \"params\": {\"name\": \"test\", \"path\": \"./test.log\"}}"
                response <- sendTestRequest socketPath "{\"method\": \"watch.list\"}"
                response `shouldContain` "\"name\":\"test\""

            it "handles unknown method gracefully" $ \(socketPath, _) -> do
                response <- sendTestRequest socketPath "{\"method\": \"unknown\"}"
                response `shouldContain` "\"ok\":false"

            it "handles malformed JSON gracefully" $ \(socketPath, _) -> do
                response <- sendTestRequest socketPath "not valid json"
                response `shouldContain` "\"ok\":false"

-- Test helpers
withTestServer :: ((FilePath, TVar DaemonState) -> IO a) -> IO a
withTestServer action =
    withSystemTempDirectory "spanshot-test" $ \dir -> do
        let socketPath = dir </> "test.sock"
        now <- getCurrentTime
        stateVar <- newTVarIO (initialState now defaultConfig)

        -- Start server in background
        serverThread <- async $ runServer socketPath stateVar

        -- Give server time to start
        threadDelay 100000 -- 100ms

        -- Run test
        result <- action (socketPath, stateVar)

        -- Cleanup
        cancel serverThread
        pure result

sendTestRequest :: FilePath -> String -> IO String
sendTestRequest socketPath request = do
    sock <- socket AF_UNIX Stream 0
    connect sock (SockAddrUnix socketPath)
    sendAll sock (BS.pack request)
    shutdown sock ShutdownSend
    response <- recv sock 4096
    close sock
    pure $ BS.unpack response
#endif
