module CaptureTypesSpec (captureTypesTests) where

import Data.Text qualified as T
import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy)

import Types (CaptureOptions (..), DetectionRule (..), defaultCaptureOptions)

captureTypesTests :: Spec
captureTypesTests = do
    describe "CaptureOptions" $ do
        it "creates default options with sensible defaults" $ do
            let opts = defaultCaptureOptions
            preWindowDuration opts `shouldBe` 5
            postWindowDuration opts `shouldBe` 5
            length (detectionRules opts) `shouldBe` 1

        it "default options include ERROR pattern" $ do
            let opts = defaultCaptureOptions
            let rules = detectionRules opts
            rules `shouldSatisfy` any (\(RegexRule p) -> p == "ERROR")
