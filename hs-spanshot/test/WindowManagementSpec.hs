module WindowManagementSpec (windowManagementTests) where

import Data.Foldable (toList)
import Data.Sequence qualified as Seq
import Data.Text qualified as T
import Test.Hspec (Spec, describe, it, shouldBe)

import Capture (addToPreWindow)
import Fixtures (mockEvent)
import Types (CaptureOptions (minContextEvents, preWindowDuration), defaultCaptureOptions)

windowManagementTests :: Spec
windowManagementTests = do
    describe "Pre-window management" $ do
        it "adds event to empty pre-window" $ do
            let opts = defaultCaptureOptions
            let event = mockEvent 1 (T.pack "INFO message")
            let result = addToPreWindow opts Seq.empty event

            Seq.length result `shouldBe` 1

        it "drops events older than preWindowDuration" $ do
            let opts = defaultCaptureOptions{preWindowDuration = 5, minContextEvents = 1}
            let old = mockEvent 0 (T.pack "old")
            let recent = mockEvent 6 (T.pack "recent")
            let current = mockEvent 10 (T.pack "current")
            let buffer = Seq.fromList [old, recent]

            let result = addToPreWindow opts buffer current

            toList result `shouldBe` [recent, current]

        it "keeps minContextEvents even if all expired" $ do
            let opts = defaultCaptureOptions{preWindowDuration = 5, minContextEvents = 3}
            let e1 = mockEvent 0 (T.pack "e1")
            let e2 = mockEvent 1 (T.pack "e2")
            let e3 = mockEvent 2 (T.pack "e3")
            let current = mockEvent 100 (T.pack "current")
            let buffer = Seq.fromList [e1, e2, e3]

            let result = addToPreWindow opts buffer current

            Seq.length result `shouldBe` 3
            toList result `shouldBe` [e2, e3, current]

        it "applies time filter when enough events remain" $ do
            let opts = defaultCaptureOptions{preWindowDuration = 5, minContextEvents = 2}
            let e1 = mockEvent 0 (T.pack "old")
            let e2 = mockEvent 6 (T.pack "recent1")
            let e3 = mockEvent 7 (T.pack "recent2")
            let e4 = mockEvent 8 (T.pack "recent3")
            let current = mockEvent 10 (T.pack "current")
            let buffer = Seq.fromList [e1, e2, e3, e4]

            let result = addToPreWindow opts buffer current

            toList result `shouldBe` [e2, e3, e4, current]
