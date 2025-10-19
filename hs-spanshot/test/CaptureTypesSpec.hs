{-# LANGUAGE OverloadedStrings #-}

module CaptureTypesSpec (captureTypesTests) where

import Data.Sequence qualified as Seq
import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy)

import Fixtures (mockEvent)
import Types (
    ActiveCapture (ActiveCapture, acDetectedBy, acErrorEvent, acPostEvents, acPreWindowSnapshot),
    CaptureState (csActiveCapture, csPreWindow),
    DetectionRule (RegexRule),
    captureDetectionRules,
    captureMinContextEvents,
    capturePostWindowDuration,
    capturePreWindowDuration,
    defaultCaptureOptions,
    initialCaptureState,
    mkCaptureOptions,
 )

captureTypesTests :: Spec
captureTypesTests = do
    describe "CaptureOptions" $ do
        it "creates default options with sensible defaults" $ do
            let opts = defaultCaptureOptions
            capturePreWindowDuration opts `shouldBe` 5
            capturePostWindowDuration opts `shouldBe` 5
            captureMinContextEvents opts `shouldBe` 10
            length (captureDetectionRules opts) `shouldBe` 1

        it "default options include ERROR pattern" $ do
            let opts = defaultCaptureOptions
            let rules = captureDetectionRules opts
            rules `shouldSatisfy` any (\(RegexRule p) -> p == "ERROR")

        it "creates options with minContextEvents" $ do
            let Right opts = mkCaptureOptions 5 5 10 [RegexRule "ERROR"]
            captureMinContextEvents opts `shouldBe` 10

    describe "ActiveCapture" $ do
        it "creates capture with snapshot and empty post-window" $ do
            let err = mockEvent 5 ("ERROR occurred")
            let snapshot = Seq.fromList [mockEvent 1 ("INFO"), mockEvent 2 ("INFO")]
            let capture = ActiveCapture{acErrorEvent = err, acDetectedBy = [RegexRule "ERROR"], acPreWindowSnapshot = snapshot, acPostEvents = Seq.empty}
            Seq.length (acPostEvents capture) `shouldBe` 0
            Seq.length (acPreWindowSnapshot capture) `shouldBe` 2

    describe "CaptureState" $ do
        it "initializes with empty state" $ do
            let state = initialCaptureState
            Seq.length (csPreWindow state) `shouldBe` 0
            csActiveCapture state `shouldBe` Nothing
