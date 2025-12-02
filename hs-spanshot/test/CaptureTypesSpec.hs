{-# LANGUAGE OverloadedStrings #-}

module CaptureTypesSpec (captureTypesTests) where

import Data.Either (isLeft, isRight)
import Data.Sequence qualified as Seq
import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy)

import Fixtures (mockEvent)
import Types (
    ActiveCapture (ActiveCapture, acDetectedBy, acErrorEvent, acPostEvents, acPreWindowSnapshot),
    CaptureOptions (detectionRules, minContextEvents, postWindowDuration, preWindowDuration),
    CaptureState (csActiveCapture, csPreWindow),
    DetectionRule (RegexRule),
    defaultCaptureOptions,
    initialCaptureState,
    mkCaptureOptions,
 )

captureTypesTests :: Spec
captureTypesTests = do
    describe "CaptureOptions" $ do
        it "creates default options with sensible defaults" $ do
            let opts = defaultCaptureOptions
            preWindowDuration opts `shouldBe` 5
            postWindowDuration opts `shouldBe` 5
            minContextEvents opts `shouldBe` 10
            length (detectionRules opts) `shouldBe` 1

        it "default options include ERROR pattern" $ do
            let opts = defaultCaptureOptions
            let rules = detectionRules opts
            rules `shouldSatisfy` any (\(RegexRule p) -> p == "ERROR")

        it "creates options with minContextEvents" $ do
            case mkCaptureOptions 5 5 10 [RegexRule "ERROR"] of
                Right opts -> minContextEvents opts `shouldBe` 10
                Left err -> fail $ "Expected Right but got Left: " <> err

        it "rejects negative preWindowDuration" $ do
            let result = mkCaptureOptions (-1) 5 10 [RegexRule "ERROR"]
            result `shouldSatisfy` isLeft

        it "rejects negative postWindowDuration" $ do
            let result = mkCaptureOptions 5 (-1) 10 [RegexRule "ERROR"]
            result `shouldSatisfy` isLeft

        it "rejects minContextEvents less than 1" $ do
            let result = mkCaptureOptions 5 5 0 [RegexRule "ERROR"]
            result `shouldSatisfy` isLeft

        it "rejects both windows being zero" $ do
            let result = mkCaptureOptions 0 0 10 [RegexRule "ERROR"]
            result `shouldSatisfy` isLeft

        it "accepts zero preWindowDuration with positive postWindowDuration" $ do
            let result = mkCaptureOptions 0 5 10 [RegexRule "ERROR"]
            result `shouldSatisfy` isRight

        it "accepts zero postWindowDuration with positive preWindowDuration" $ do
            let result = mkCaptureOptions 5 0 10 [RegexRule "ERROR"]
            result `shouldSatisfy` isRight

        it "rejects empty detection rules" $ do
            let result = mkCaptureOptions 5 5 10 []
            result `shouldSatisfy` isLeft

        it "rejects invalid regex patterns" $ do
            let result = mkCaptureOptions 5 5 10 [RegexRule "[invalid"]
            result `shouldSatisfy` isLeft

        it "accepts valid regex patterns" $ do
            let result = mkCaptureOptions 5 5 10 [RegexRule "ERROR|FATAL", RegexRule "\\[ERROR\\]"]
            result `shouldSatisfy` isRight

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
