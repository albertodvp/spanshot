module DaemonStateSpec (daemonStateTests) where

import Data.Map.Strict qualified as Map
import Data.Sequence qualified as Seq
import Data.Time (UTCTime)
import Data.Time.Calendar (fromGregorian)
import Data.Time.Clock (UTCTime (..), secondsToDiffTime)
import Test.Hspec

import Config (defaultConfig)
import Daemon.State
import Daemon.Types (Watch (..))
import Types (CollectEvent (..), DetectionRule (..), SpanShot (..))

daemonStateTests :: Spec
daemonStateTests = do
    describe "DaemonState" $ do
        describe "initialState" $ do
            it "has empty watches" $ do
                watches (initialState someTime defaultConfig) `shouldBe` Map.empty

            it "has empty spanShots" $ do
                spanShots (initialState someTime defaultConfig) `shouldBe` Seq.empty

            it "records start time" $ do
                startTime (initialState someTime defaultConfig) `shouldBe` someTime

        describe "addWatch" $ do
            it "adds a watch to state" $ do
                let state = initialState someTime defaultConfig
                let state' = addWatch "backend" (FileWatch "./app.log") state
                Map.member "backend" (watches state') `shouldBe` True

            it "overwrites existing watch with same name" $ do
                let state = initialState someTime defaultConfig
                let state' = addWatch "backend" (FileWatch "./old.log") state
                let state'' = addWatch "backend" (FileWatch "./new.log") state'
                Map.size (watches state'') `shouldBe` 1

        describe "removeWatch" $ do
            it "removes existing watch" $ do
                let state =
                        addWatch "backend" (FileWatch "./app.log") $
                            initialState someTime defaultConfig
                let state' = removeWatch "backend" state
                Map.member "backend" (watches state') `shouldBe` False

            it "no-op for non-existent watch" $ do
                let state = initialState someTime defaultConfig
                let state' = removeWatch "nonexistent" state
                state' `shouldBe` state

        describe "addSpanShot" $ do
            it "adds spanshot to state" $ do
                let state = initialState someTime defaultConfig
                let state' = addSpanShot someSpanShot state
                Seq.length (spanShots state') `shouldBe` 1

            it "preserves order (newest last)" $ do
                let state = initialState someTime defaultConfig
                let state' = addSpanShot shot2 state
                let state'' = addSpanShot shot1 state'
                -- shot2 added first, shot1 added second
                Seq.index (spanShots state'') 1 `shouldBe` shot1

        describe "clearSpanShots" $ do
            it "removes all spanshots" $ do
                let state =
                        addSpanShot someSpanShot $
                            initialState someTime defaultConfig
                let state' = clearSpanShots state
                spanShots state' `shouldBe` Seq.empty

        describe "updateConfig" $ do
            it "updates config in state" $ do
                let state = initialState someTime defaultConfig
                let state' = updateConfig defaultConfig state
                config state' `shouldBe` defaultConfig

        describe "getUptime" $ do
            it "calculates uptime in seconds" $ do
                let state = initialState someTime defaultConfig
                getUptime laterTime state `shouldBe` 3600 -- 1 hour

-- Test fixtures
someTime :: UTCTime
someTime = UTCTime (fromGregorian 2025 1 15) (secondsToDiffTime 36000) -- 10:00:00

laterTime :: UTCTime
laterTime = UTCTime (fromGregorian 2025 1 15) (secondsToDiffTime 39600) -- 11:00:00

someCollectEvent :: CollectEvent
someCollectEvent =
    CollectEvent
        { source = "./test.log"
        , sessionOrderId = 0
        , readAtUtc = someTime
        , line = "ERROR: Test error"
        }

someSpanShot :: SpanShot
someSpanShot =
    SpanShot
        { errorEvent = someCollectEvent
        , preWindow = []
        , postWindow = []
        , detectedBy = [RegexRule "ERROR"]
        , capturedAtUtc = someTime
        }

shot1 :: SpanShot
shot1 = someSpanShot{errorEvent = someCollectEvent{sessionOrderId = 1}}

shot2 :: SpanShot
shot2 = someSpanShot{errorEvent = someCollectEvent{sessionOrderId = 2}}
