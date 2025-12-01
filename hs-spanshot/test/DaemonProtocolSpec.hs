module DaemonProtocolSpec (daemonProtocolTests) where

import Data.Aeson (eitherDecode, encode)
import Data.ByteString.Lazy.Char8 qualified as BL
import Data.Either (isLeft)
import Test.Hspec

import Daemon.Protocol
import Daemon.Types (DaemonStats (..))

daemonProtocolTests :: Spec
daemonProtocolTests = do
    describe "Request parsing" $ do
        it "parses status request" $ do
            let json = "{\"method\": \"status\"}"
            eitherDecode json `shouldBe` Right ReqStatus

        it "parses shutdown request" $ do
            let json = "{\"method\": \"shutdown\"}"
            eitherDecode json `shouldBe` Right ReqShutdown

        it "parses watch.add request" $ do
            let json = "{\"method\": \"watch.add\", \"params\": {\"name\": \"backend\", \"path\": \"./app.log\"}}"
            eitherDecode json `shouldBe` Right (ReqWatchAdd "backend" "./app.log")

        it "parses watch.list request" $ do
            let json = "{\"method\": \"watch.list\"}"
            eitherDecode json `shouldBe` Right ReqWatchList

        it "parses watch.remove request" $ do
            let json = "{\"method\": \"watch.remove\", \"params\": {\"name\": \"backend\"}}"
            eitherDecode json `shouldBe` Right (ReqWatchRemove "backend")

        it "parses errors.list request" $ do
            let json = "{\"method\": \"errors.list\", \"params\": {\"limit\": 10}}"
            eitherDecode json `shouldBe` Right (ReqErrorsList 10)

        it "parses errors.list with default limit" $ do
            let json = "{\"method\": \"errors.list\"}"
            eitherDecode json `shouldBe` Right (ReqErrorsList 50) -- default

        it "parses errors.show request" $ do
            let json = "{\"method\": \"errors.show\", \"params\": {\"id\": 5}}"
            eitherDecode json `shouldBe` Right (ReqErrorsShow 5)

        it "parses errors.clear request" $ do
            let json = "{\"method\": \"errors.clear\"}"
            eitherDecode json `shouldBe` Right ReqErrorsClear

        it "parses config.get request" $ do
            let json = "{\"method\": \"config.get\"}"
            eitherDecode json `shouldBe` Right ReqConfigGet

        it "fails on unknown method" $ do
            let json = "{\"method\": \"unknown\"}"
            (eitherDecode json :: Either String Request) `shouldSatisfy` isLeft

    describe "Response serialization" $ do
        it "serializes success response" $ do
            let stats = DaemonStats 3600 2 5 12345 "/path/to/config.yaml"
            let resp = RespOk (ResultStatus stats)
            let json = encode resp
            BL.unpack json `shouldContain` "\"ok\":true"
            BL.unpack json `shouldContain` "\"uptime\":3600"

        it "serializes error response" $ do
            let resp = RespError "Watch not found"
            let json = encode resp
            BL.unpack json `shouldContain` "\"ok\":false"
            BL.unpack json `shouldContain` "\"error\":\"Watch not found\""

    describe "Request serialization (for client)" $ do
        it "serializes status request" $ do
            let json = encode ReqStatus
            BL.unpack json `shouldContain` "\"method\":\"status\""

        it "serializes watch.add request" $ do
            let json = encode (ReqWatchAdd "backend" "./app.log")
            BL.unpack json `shouldContain` "\"method\":\"watch.add\""
            BL.unpack json `shouldContain` "\"name\":\"backend\""

    describe "Round-trip" $ do
        it "round-trips status request" $ do
            let req = ReqStatus
            eitherDecode (encode req) `shouldBe` Right req

        it "round-trips watch.add request" $ do
            let req = ReqWatchAdd "test" "/var/log/test.log"
            eitherDecode (encode req) `shouldBe` Right req

        it "round-trips errors.list request" $ do
            let req = ReqErrorsList 25
            eitherDecode (encode req) `shouldBe` Right req
