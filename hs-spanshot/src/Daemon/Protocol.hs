{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}

module Daemon.Protocol (
    -- * Request types
    Request (..),
    -- * Response types
    Response (..),
    ResponseResult (..),
    -- * Parsing/encoding
    parseRequest,
    encodeRequest,
    parseResponse,
    encodeResponse,
) where

import Data.Aeson
import Data.Aeson.KeyMap qualified as KM
import Data.Aeson.Types (Parser)
import Data.ByteString.Lazy (ByteString)
import Data.Text (Text)
import GHC.Generics (Generic)

import Config (Config)
import Daemon.Types (DaemonStats (..), WatchName)
import Types (SpanShot)

-- | IPC Requests
data Request
    = ReqStatus
    | ReqShutdown
    | ReqConfigGet
    | ReqWatchAdd !WatchName !FilePath
    | ReqWatchList
    | ReqWatchRemove !WatchName
    | ReqErrorsList !Int -- limit
    | ReqErrorsShow !Int -- id
    | ReqErrorsClear
    deriving (Show, Eq)

instance FromJSON Request where
    parseJSON = withObject "Request" $ \o -> do
        method <- o .: "method"
        params <- o .:? "params" .!= KM.empty
        parseMethod method params
      where
        parseMethod :: Text -> Object -> Parser Request
        parseMethod "status" _ = pure ReqStatus
        parseMethod "shutdown" _ = pure ReqShutdown
        parseMethod "config.get" _ = pure ReqConfigGet
        parseMethod "watch.add" p = ReqWatchAdd <$> p .: "name" <*> p .: "path"
        parseMethod "watch.list" _ = pure ReqWatchList
        parseMethod "watch.remove" p = ReqWatchRemove <$> p .: "name"
        parseMethod "errors.list" p = ReqErrorsList <$> p .:? "limit" .!= 50
        parseMethod "errors.show" p = ReqErrorsShow <$> p .: "id"
        parseMethod "errors.clear" _ = pure ReqErrorsClear
        parseMethod m _ = fail $ "Unknown method: " ++ show m

instance ToJSON Request where
    toJSON ReqStatus = object ["method" .= ("status" :: Text)]
    toJSON ReqShutdown = object ["method" .= ("shutdown" :: Text)]
    toJSON ReqConfigGet = object ["method" .= ("config.get" :: Text)]
    toJSON (ReqWatchAdd name path) =
        object
            [ "method" .= ("watch.add" :: Text)
            , "params" .= object ["name" .= name, "path" .= path]
            ]
    toJSON ReqWatchList = object ["method" .= ("watch.list" :: Text)]
    toJSON (ReqWatchRemove name) =
        object
            [ "method" .= ("watch.remove" :: Text)
            , "params" .= object ["name" .= name]
            ]
    toJSON (ReqErrorsList limit) =
        object
            [ "method" .= ("errors.list" :: Text)
            , "params" .= object ["limit" .= limit]
            ]
    toJSON (ReqErrorsShow idx) =
        object
            [ "method" .= ("errors.show" :: Text)
            , "params" .= object ["id" .= idx]
            ]
    toJSON ReqErrorsClear = object ["method" .= ("errors.clear" :: Text)]

-- | IPC Responses
data Response
    = RespOk !ResponseResult
    | RespError !Text
    deriving (Show, Eq)

instance ToJSON Response where
    toJSON (RespOk result) = object ["ok" .= True, "result" .= result]
    toJSON (RespError msg) = object ["ok" .= False, "error" .= msg]

instance FromJSON Response where
    parseJSON = withObject "Response" $ \o -> do
        ok <- o .: "ok"
        if ok
            then RespOk <$> o .: "result"
            else RespError <$> o .: "error"

-- | Response payloads
data ResponseResult
    = ResultStatus !DaemonStats
    | ResultOk -- Simple acknowledgment
    | ResultWatchList ![(WatchName, FilePath)]
    | ResultErrorsList ![SpanShot]
    | ResultErrorsShow !SpanShot
    | ResultConfig !Config
    deriving (Show, Eq, Generic)

instance ToJSON ResponseResult where
    toJSON (ResultStatus stats) = toJSON stats
    toJSON ResultOk = object []
    toJSON (ResultWatchList ws) = object ["watches" .= map toWatchObj ws]
      where
        toWatchObj (n, p) = object ["name" .= n, "path" .= p]
    toJSON (ResultErrorsList shots) = object ["errors" .= shots]
    toJSON (ResultErrorsShow shot) = toJSON shot
    toJSON (ResultConfig cfg) = toJSON cfg

instance FromJSON ResponseResult where
    parseJSON = withObject "Result" $ \o -> do
        -- Try to parse as different result types based on keys present
        case KM.lookup "watches" o of
            Just _ -> do
                watches <- o .: "watches"
                watchList <- mapM parseWatch watches
                pure $ ResultWatchList watchList
            Nothing -> case KM.lookup "errors" o of
                Just _ -> ResultErrorsList <$> o .: "errors"
                Nothing -> case KM.lookup "uptime" o of
                    Just _ -> ResultStatus <$> parseJSON (Object o)
                    Nothing -> case KM.lookup "error_event" o of
                        Just _ -> ResultErrorsShow <$> parseJSON (Object o)
                        Nothing -> case KM.lookup "capture" o of
                            Just _ -> ResultConfig <$> parseJSON (Object o)
                            Nothing -> pure ResultOk
      where
        parseWatch = withObject "Watch" $ \w ->
            (,) <$> w .: "name" <*> w .: "path"

-- | Parse a request from bytes
parseRequest :: ByteString -> Either String Request
parseRequest = eitherDecode

-- | Encode a request to bytes (for client use)
encodeRequest :: Request -> ByteString
encodeRequest = encode

-- | Parse a response from bytes
parseResponse :: ByteString -> Either String Response
parseResponse = eitherDecode

-- | Encode a response to bytes (for server use)
encodeResponse :: Response -> ByteString
encodeResponse = encode
