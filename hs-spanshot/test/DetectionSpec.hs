module DetectionSpec (detectionTests) where

import Data.Text qualified as T
import Test.Hspec (Spec, describe, it, shouldBe)

import Capture (detectError, runAllDetectors)
import Fixtures (mockEvent)
import Types (DetectionRule (..))

detectionTests :: Spec
detectionTests = do
    describe "detectError" $ do
        it "detects ERROR in log line" $ do
            let rule = RegexRule "ERROR"
            let event = mockEvent 1 (T.pack "2025-10-14 10:15:37 ERROR Failed to process")
            detectError rule event `shouldBe` True

        it "returns False for non-matching line" $ do
            let rule = RegexRule "ERROR"
            let event = mockEvent 1 (T.pack "INFO All good")
            detectError rule event `shouldBe` False

        it "detects different case variations with separate patterns" $ do
            let ruleUpper = RegexRule "ERROR"
            let ruleLower = RegexRule "error"
            let event1 = mockEvent 1 (T.pack "ERROR occurred")
            let event2 = mockEvent 2 (T.pack "error occurred")
            detectError ruleUpper event1 `shouldBe` True
            detectError ruleLower event2 `shouldBe` True

        it "handles special regex characters" $ do
            let rule = RegexRule "\\[ERROR\\]"
            let event = mockEvent 1 (T.pack "[ERROR] Something went wrong")
            detectError rule event `shouldBe` True

        it "handles complex regex patterns" $ do
            let rule = RegexRule "(ERROR|FATAL|EXCEPTION)"
            let event1 = mockEvent 1 (T.pack "ERROR: connection failed")
            let event2 = mockEvent 2 (T.pack "FATAL crash detected")
            let event3 = mockEvent 3 (T.pack "EXCEPTION in thread")
            let event4 = mockEvent 4 (T.pack "INFO nothing wrong")
            detectError rule event1 `shouldBe` True
            detectError rule event2 `shouldBe` True
            detectError rule event3 `shouldBe` True
            detectError rule event4 `shouldBe` False

        it "detects Python-style Traceback" $ do
            let rule = RegexRule "Traceback"
            let event = mockEvent 1 (T.pack "Traceback (most recent call last):")
            detectError rule event `shouldBe` True

    describe "runAllDetectors" $ do
        it "returns empty list when no rules match" $ do
            let rules = [RegexRule "ERROR", RegexRule "FATAL"]
            let event = mockEvent 1 (T.pack "INFO Everything is fine")
            runAllDetectors rules event `shouldBe` []

        it "returns single matching rule" $ do
            let rules = [RegexRule "ERROR", RegexRule "FATAL"]
            let event = mockEvent 1 (T.pack "ERROR occurred")
            let matches = runAllDetectors rules event
            length matches `shouldBe` 1
            matches `shouldBe` [RegexRule "ERROR"]

        it "returns multiple matching rules" $ do
            let rules = [RegexRule "ERROR", RegexRule "FATAL", RegexRule "E.R"]
            let event = mockEvent 1 (T.pack "FATAL ERROR")
            let matches = runAllDetectors rules event
            length matches `shouldBe` 3

        it "preserves order of rules" $ do
            let rule1 = RegexRule "A"
            let rule2 = RegexRule "B"
            let rule3 = RegexRule "C"
            let rules = [rule1, rule2, rule3]
            let event = mockEvent 1 (T.pack "ABC")
            runAllDetectors rules event `shouldBe` [rule1, rule2, rule3]
