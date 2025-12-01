{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}

module Daemon.Server (
    runServer,
    handleClient,
    handleRequest,
) where

#if !defined(mingw32_HOST_OS)
import Control.Concurrent (forkIO)
import Control.Concurrent.STM
import Control.Exception (bracket, finally)
import Control.Monad (forever, void, when)
import Data.ByteString.Lazy qualified as BL
import Data.Map.Strict qualified as Map
import Data.Sequence qualified as Seq
import Data.Text qualified as T
import Data.Time (getCurrentTime)
import Network.Socket
import Network.Socket.ByteString (recv, sendAll)
import System.Directory (doesFileExist, removeFile)
import System.Posix.Process (getProcessID)

import Config (getConfigPath)
import Daemon.Protocol
import Daemon.State
import Daemon.Types

-- | Run the IPC server on a Unix socket
runServer :: FilePath -> TVar DaemonState -> IO ()
runServer socketPath stateVar = do
    -- Remove stale socket if exists
    removeIfExists socketPath

    -- Create and bind socket
    bracket (socket AF_UNIX Stream 0) close $ \sock -> do
        bind sock (SockAddrUnix socketPath)
        listen sock 5

        -- Accept loop
        forever $ do
            (clientSock, _) <- accept sock
            -- Handle each client in separate thread
            void $ forkIO $ handleClient clientSock stateVar `finally` close clientSock

-- | Handle a single client connection
handleClient :: Socket -> TVar DaemonState -> IO ()
handleClient sock stateVar = do
    -- Read request
    requestBytes <- recv sock 4096

    -- Parse and handle
    response <- case parseRequest (BL.fromStrict requestBytes) of
        Left err -> pure $ RespError $ "Parse error: " <> T.pack err
        Right req -> handleRequest stateVar req

    -- Send response
    sendAll sock (BL.toStrict $ encodeResponse response)

-- | Handle a parsed request
handleRequest :: TVar DaemonState -> Request -> IO Response
handleRequest stateVar = \case
    ReqStatus -> do
        state <- readTVarIO stateVar
        now <- getCurrentTime
        pid <- getProcessID
        configPath <- getConfigPath
        let stats =
                DaemonStats
                    { statsUptime = getUptime now state
                    , statsWatchCount = Map.size (watches state)
                    , statsErrorCount = Seq.length (spanShots state)
                    , statsPid = fromIntegral pid
                    , statsConfigPath = configPath
                    }
        pure $ RespOk (ResultStatus stats)
    ReqShutdown -> do
        -- Signal will be handled by caller
        pure $ RespOk ResultOk
    ReqWatchAdd name path -> do
        atomically $ modifyTVar' stateVar (addWatch name (FileWatch path))
        pure $ RespOk ResultOk
    ReqWatchList -> do
        state <- readTVarIO stateVar
        let ws = [(n, p) | (n, FileWatch p) <- listWatches state]
        pure $ RespOk (ResultWatchList ws)
    ReqWatchRemove name -> do
        atomically $ modifyTVar' stateVar (removeWatch name)
        pure $ RespOk ResultOk
    ReqErrorsList limit -> do
        state <- readTVarIO stateVar
        pure $ RespOk (ResultErrorsList $ listSpanShots limit state)
    ReqErrorsShow idx -> do
        state <- readTVarIO stateVar
        case getSpanShot idx state of
            Just shot -> pure $ RespOk (ResultErrorsShow shot)
            Nothing -> pure $ RespError "Error not found"
    ReqErrorsClear -> do
        atomically $ modifyTVar' stateVar clearSpanShots
        pure $ RespOk ResultOk
    ReqConfigGet -> do
        state <- readTVarIO stateVar
        pure $ RespOk (ResultConfig $ config state)

removeIfExists :: FilePath -> IO ()
removeIfExists path = do
    exists <- doesFileExist path
    when exists $ removeFile path
#else
-- Windows stub
import Control.Concurrent.STM

import Daemon.Types

runServer :: FilePath -> TVar DaemonState -> IO ()
runServer _ _ = error "Daemon mode not supported on Windows"

handleClient :: a -> TVar DaemonState -> IO ()
handleClient _ _ = error "Daemon mode not supported on Windows"

handleRequest :: TVar DaemonState -> a -> IO b
handleRequest _ _ = error "Daemon mode not supported on Windows"
#endif
