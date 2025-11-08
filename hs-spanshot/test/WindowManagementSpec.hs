{-# LANGUAGE OverloadedStrings #-}

module WindowManagementSpec (windowManagementTests) where

import Data.Foldable (toList)
import Data.Sequence qualified as Seq
import Test.Hspec (Spec, describe, expectationFailure, it, shouldBe)

import Capture (addToPreWindow)
import Fixtures (mockEvent)
import Types (CaptureOptions (detectionRules, postWindowDuration), defaultCaptureOptions, mkCaptureOptions)

windowManagementTests :: Spec
windowManagementTests = do
    describe "Pre-window management" $ do
        it "adds event to empty pre-window" $ do
            let opts = defaultCaptureOptions
            let event = mockEvent 1 ("INFO message")
            let result = addToPreWindow opts Seq.empty event

            Seq.length result `shouldBe` 1

        it "drops events older than preWindowDuration" $ do
            opts <- case mkCaptureOptions 5 (postWindowDuration defaultCaptureOptions) 1 (detectionRules defaultCaptureOptions) of
                Right o -> pure o
                Left err -> fail $ "Invalid options: " <> err
            let old = mockEvent 0 ("old")
            let recent = mockEvent 6 ("recent")
            let current = mockEvent 10 ("current")
            let buffer = Seq.fromList [old, recent]

            let result = addToPreWindow opts buffer current

            toList result `shouldBe` [recent, current]

        it "keeps minContextEvents even if all expired" $ do
            opts <- case mkCaptureOptions 5 (postWindowDuration defaultCaptureOptions) 3 (detectionRules defaultCaptureOptions) of
                Right o -> pure o
                Left err -> fail $ "Invalid options: " <> err
            let e1 = mockEvent 0 ("e1")
            let e2 = mockEvent 1 ("e2")
            let e3 = mockEvent 2 ("e3")
            let current = mockEvent 100 ("current")
            let buffer = Seq.fromList [e1, e2, e3]

            let result = addToPreWindow opts buffer current

            Seq.length result `shouldBe` 3
            toList result `shouldBe` [e2, e3, current]

        it "applies time filter when enough events remain" $ do
            opts <- case mkCaptureOptions 5 (postWindowDuration defaultCaptureOptions) 2 (detectionRules defaultCaptureOptions) of
                Right o -> pure o
                Left err -> fail $ "Invalid options: " <> err
            let e1 = mockEvent 0 ("old")
            let e2 = mockEvent 6 ("recent1")
            let e3 = mockEvent 7 ("recent2")
            let e4 = mockEvent 8 ("recent3")
            let current = mockEvent 10 ("current")
            let buffer = Seq.fromList [e1, e2, e3, e4]

            let result = addToPreWindow opts buffer current

            toList result `shouldBe` [e2, e3, e4, current]

        it "keeps events exactly at the time boundary" $ do
            opts <- case mkCaptureOptions 5 (postWindowDuration defaultCaptureOptions) 1 (detectionRules defaultCaptureOptions) of
                Right o -> pure o
                Left err -> fail $ "Invalid options: " <> err
            let boundary = mockEvent 5 ("boundary")
            let current = mockEvent 10 ("current")
            let buffer = Seq.fromList [boundary]

            let result = addToPreWindow opts buffer current

            toList result `shouldBe` [boundary, current]
