{-# LANGUAGE CPP #-}

module Client (
    sendCommand,
    ClientError (..),
) where

#if !defined(mingw32_HOST_OS)
import Control.Exception (IOException, try)
import Data.ByteString.Lazy qualified as BL
import Network.Socket
import Network.Socket.ByteString (recv, sendAll)

import Daemon.Process (getSocketPath)
import Daemon.Protocol (Request, Response, encodeRequest, parseResponse)

data ClientError
    = ConnectionFailed String
    | ProtocolError String
    deriving (Show, Eq)

sendCommand :: Request -> IO (Either ClientError Response)
sendCommand request = do
    socketPath <- getSocketPath
    result <- try $ do
        sock <- socket AF_UNIX Stream 0
        connect sock (SockAddrUnix socketPath)
        sendAll sock (BL.toStrict $ encodeRequest request)
        shutdown sock ShutdownSend
        responseBytes <- recv sock 65536
        close sock
        pure responseBytes

    case result of
        Left (e :: IOException) ->
            pure $ Left $ ConnectionFailed $ show e
        Right bytes ->
            case parseResponse (BL.fromStrict bytes) of
                Left err -> pure $ Left $ ProtocolError err
                Right resp -> pure $ Right resp
#else
data ClientError
    = ConnectionFailed String
    | ProtocolError String
    deriving (Show, Eq)

sendCommand :: a -> IO (Either ClientError b)
sendCommand _ = pure $ Left $ ConnectionFailed "Daemon mode not supported on Windows"
#endif
