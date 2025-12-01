{-# LANGUAGE OverloadedStrings #-}

module WindowManagementSpec (windowManagementTests) where

import Data.Foldable (toList)
import Data.Sequence qualified as Seq
import Data.Time (NominalDiffTime)
import Test.Hspec (Spec, describe, it, shouldBe)

import Capture (addToPreWindow)
import Fixtures (mockEvent)
import Types (CaptureOptions, DetectionRule (..), mkCaptureOptions)

-- | Explicit test constants for window management tests
testPostWindowSeconds :: NominalDiffTime
testPostWindowSeconds = 5

testDetectionRules :: [DetectionRule]
testDetectionRules = [RegexRule "ERROR"]

{- | Helper to create test options with specific pre-window and minContext values
preWinSec: pre-window duration in seconds
minCtx: minimum context events to keep
-}
mkTestOpts :: NominalDiffTime -> Int -> CaptureOptions
mkTestOpts preWinSec minCtx = case mkCaptureOptions preWinSec testPostWindowSeconds minCtx testDetectionRules of
    Right opts -> opts
    Left err -> error $ "Invalid test options: " <> err

windowManagementTests :: Spec
windowManagementTests = do
    describe "Pre-window management" $ do
        it "adds event to empty pre-window" $ do
            let opts = mkTestOpts 5 10
            let event = mockEvent 1 ("INFO message")
            let result = addToPreWindow opts Seq.empty event

            Seq.length result `shouldBe` 1

        it "drops events older than preWindowDuration" $ do
            let opts = mkTestOpts 5 1
            let old = mockEvent 0 ("old")
            let recent = mockEvent 6 ("recent")
            let current = mockEvent 10 ("current")
            let buffer = Seq.fromList [old, recent]

            let result = addToPreWindow opts buffer current

            toList result `shouldBe` [recent, current]

        it "keeps minContextEvents even if all expired" $ do
            let opts = mkTestOpts 5 3
            let e1 = mockEvent 0 ("e1")
            let e2 = mockEvent 1 ("e2")
            let e3 = mockEvent 2 ("e3")
            let current = mockEvent 100 ("current")
            let buffer = Seq.fromList [e1, e2, e3]

            let result = addToPreWindow opts buffer current

            Seq.length result `shouldBe` 3
            toList result `shouldBe` [e2, e3, current]

        it "applies time filter when enough events remain" $ do
            let opts = mkTestOpts 5 2
            let e1 = mockEvent 0 ("old")
            let e2 = mockEvent 6 ("recent1")
            let e3 = mockEvent 7 ("recent2")
            let e4 = mockEvent 8 ("recent3")
            let current = mockEvent 10 ("current")
            let buffer = Seq.fromList [e1, e2, e3, e4]

            let result = addToPreWindow opts buffer current

            toList result `shouldBe` [e2, e3, e4, current]

        it "keeps events exactly at the time boundary" $ do
            let opts = mkTestOpts 5 1
            let boundary = mockEvent 5 ("boundary")
            let current = mockEvent 10 ("current")
            let buffer = Seq.fromList [boundary]

            let result = addToPreWindow opts buffer current

            toList result `shouldBe` [boundary, current]
