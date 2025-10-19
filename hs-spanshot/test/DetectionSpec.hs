module DetectionSpec (detectionTests) where

import Test.Hspec (Spec, describe, it, shouldBe)

import Capture (detectError, runAllDetectors)
import Fixtures (mockEvent)
import Types (DetectionRule (RegexRule))

detectionTests :: Spec
detectionTests = do
    describe "detectError" $ do
        it "detects ERROR in log line" $ do
            let rule = RegexRule "ERROR"
            let event = mockEvent 1 "2025-10-14 10:15:37 ERROR Failed to process"
            detectError rule event `shouldBe` True

        it "returns False for non-matching line" $ do
            let rule = RegexRule "ERROR"
            let event = mockEvent 1 "INFO All good"
            detectError rule event `shouldBe` False

        it "detects different case variations with separate patterns" $ do
            let ruleUpper = RegexRule "ERROR"
            let ruleLower = RegexRule "error"
            let event1 = mockEvent 1 "ERROR occurred"
            let event2 = mockEvent 2 "error occurred"
            detectError ruleUpper event1 `shouldBe` True
            detectError ruleLower event2 `shouldBe` True

        it "handles special regex characters" $ do
            let rule = RegexRule "\\[ERROR\\]"
            let event = mockEvent 1 "[ERROR] Something went wrong"
            detectError rule event `shouldBe` True

        it "handles complex regex patterns" $ do
            let rule = RegexRule "(ERROR|FATAL|EXCEPTION)"
            let event1 = mockEvent 1 "ERROR: connection failed"
            let event2 = mockEvent 2 "FATAL crash detected"
            let event3 = mockEvent 3 "EXCEPTION in thread"
            let event4 = mockEvent 4 "INFO nothing wrong"
            detectError rule event1 `shouldBe` True
            detectError rule event2 `shouldBe` True
            detectError rule event3 `shouldBe` True
            detectError rule event4 `shouldBe` False

        it "detects Python-style Traceback" $ do
            let rule = RegexRule "Traceback"
            let event = mockEvent 1 "Traceback (most recent call last):"
            detectError rule event `shouldBe` True

    describe "runAllDetectors" $ do
        it "returns empty list when no rules match" $ do
            let rules = [RegexRule "ERROR", RegexRule "FATAL"]
            let event = mockEvent 1 "INFO Everything is fine"
            runAllDetectors rules event `shouldBe` []

        it "returns single matching rule" $ do
            let rules = [RegexRule "ERROR", RegexRule "FATAL"]
            let event = mockEvent 1 "ERROR occurred"
            let matches = runAllDetectors rules event
            length matches `shouldBe` 1
            matches `shouldBe` [RegexRule "ERROR"]

        it "returns multiple matching rules" $ do
            let rules = [RegexRule "ERROR", RegexRule "FATAL", RegexRule "E.R"]
            let event = mockEvent 1 "FATAL ERROR"
            let matches = runAllDetectors rules event
            length matches `shouldBe` 3

        it "preserves order of rules" $ do
            let rule1 = RegexRule "A"
            let rule2 = RegexRule "B"
            let rule3 = RegexRule "C"
            let rules = [rule1, rule2, rule3]
            let event = mockEvent 1 "ABC"
            runAllDetectors rules event `shouldBe` [rule1, rule2, rule3]
