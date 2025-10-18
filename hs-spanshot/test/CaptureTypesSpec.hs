module CaptureTypesSpec (captureTypesTests) where

import Data.Sequence qualified as Seq
import Data.Text qualified as T
import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy)

import Fixtures (mockEvent)
import Types (ActiveCapture (ActiveCapture, acDetectedBy, acErrorEvent, acPostEvents, acPreWindowSnapshot), CaptureOptions (CaptureOptions, detectionRules, minContextEvents, postWindowDuration, preWindowDuration), CaptureState (csActiveCapture, csPreWindow), DetectionRule (RegexRule), defaultCaptureOptions, initialCaptureState)

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
            let opts = CaptureOptions{preWindowDuration = 5, postWindowDuration = 5, minContextEvents = 10, detectionRules = [RegexRule "ERROR"]}
            minContextEvents opts `shouldBe` 10

    describe "ActiveCapture" $ do
        it "creates capture with snapshot and empty post-window" $ do
            let err = mockEvent 5 (T.pack "ERROR occurred")
            let snapshot = Seq.fromList [mockEvent 1 (T.pack "INFO"), mockEvent 2 (T.pack "INFO")]
            let capture = ActiveCapture{acErrorEvent = err, acDetectedBy = [RegexRule "ERROR"], acPreWindowSnapshot = snapshot, acPostEvents = Seq.empty}
            Seq.length (acPostEvents capture) `shouldBe` 0
            Seq.length (acPreWindowSnapshot capture) `shouldBe` 2

    describe "CaptureState" $ do
        it "initializes with empty state" $ do
            let state = initialCaptureState
            Seq.length (csPreWindow state) `shouldBe` 0
            csActiveCapture state `shouldBe` Nothing
