{-# LANGUAGE OverloadedStrings #-}

module CaptureTypesSpec (captureTypesTests) where

import Data.Either (isLeft, isRight)
import Data.Foldable (toList)
import Data.List (isInfixOf, isPrefixOf)
import Data.Sequence qualified as Seq
import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy)

import Fixtures (mockEvent, mockTime)
import Types (
    ActiveCapture (ActiveCapture, acDetectedBy, acErrorEvent, acPostEvents, acPreWindowSnapshot),
    CaptureOptions (compiledRules, detectionRules, inactivityTimeout, minContextEvents, postWindowDuration, preWindowDuration),
    CaptureState (csActiveCapture, csPreWindow),
    DetectionRule (RegexRule),
    SpanShot (..),
    defaultCaptureOptions,
    initialCaptureState,
    mkCaptureOptions,
    mkCaptureOptionsWithTimeout,
    spanShotFromSeq,
    spanShotToSeq,
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

        it "default options have inactivityTimeout = 2 * postWindowDuration" $ do
            let opts = defaultCaptureOptions
            inactivityTimeout opts `shouldBe` (2 * postWindowDuration opts)

        it "default options include ERROR pattern" $ do
            let opts = defaultCaptureOptions
            let rules = detectionRules opts
            rules `shouldSatisfy` any (\(RegexRule p) -> p == "ERROR")

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

        it "mkCaptureOptions defaults inactivityTimeout to 2 * postWindowDuration" $ do
            case mkCaptureOptions 5 10 10 [RegexRule "ERROR"] of
                Left err -> fail $ "Expected Right but got Left: " ++ err
                Right opts -> inactivityTimeout opts `shouldBe` 20 -- 2 * 10
        it "mkCaptureOptionsWithTimeout allows custom inactivityTimeout" $ do
            case mkCaptureOptionsWithTimeout 5 5 10 [RegexRule "ERROR"] 30 of
                Left err -> fail $ "Expected Right but got Left: " ++ err
                Right opts -> inactivityTimeout opts `shouldBe` 30

        it "rejects inactivityTimeout less than postWindowDuration" $ do
            let result = mkCaptureOptionsWithTimeout 5 10 10 [RegexRule "ERROR"] 5
            result `shouldSatisfy` isLeft

        it "accepts inactivityTimeout equal to postWindowDuration" $ do
            let result = mkCaptureOptionsWithTimeout 5 10 10 [RegexRule "ERROR"] 10
            result `shouldSatisfy` isRight

        it "rejects zero inactivityTimeout" $ do
            let result = mkCaptureOptionsWithTimeout 5 5 10 [RegexRule "ERROR"] 0
            result `shouldSatisfy` isLeft

        it "rejects negative inactivityTimeout" $ do
            let result = mkCaptureOptionsWithTimeout 5 5 10 [RegexRule "ERROR"] (-5)
            result `shouldSatisfy` isLeft

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

    describe "SpanShot conversion functions" $ do
        it "spanShotToSeq extracts components correctly" $ do
            let err = mockEvent 5 "ERROR occurred"
            let pre = [mockEvent 1 "pre1", mockEvent 2 "pre2"]
            let post = [mockEvent 6 "post1", mockEvent 7 "post2"]
            let rules = [RegexRule "ERROR"]
            let time = mockTime 10
            let shot = SpanShot err pre post rules time
            let (errOut, preSeq, postSeq) = spanShotToSeq shot
            errOut `shouldBe` err
            toList preSeq `shouldBe` pre
            toList postSeq `shouldBe` post

        it "spanShotFromSeq constructs SpanShot correctly" $ do
            let err = mockEvent 5 "ERROR occurred"
            let preSeq = Seq.fromList [mockEvent 1 "pre1", mockEvent 2 "pre2"]
            let postSeq = Seq.fromList [mockEvent 6 "post1"]
            let rules = [RegexRule "ERROR", RegexRule "FATAL"]
            let time = mockTime 10
            let shot = spanShotFromSeq err preSeq postSeq rules time
            errorEvent shot `shouldBe` err
            preWindow shot `shouldBe` toList preSeq
            postWindow shot `shouldBe` toList postSeq
            detectedBy shot `shouldBe` rules
            capturedAtUtc shot `shouldBe` time

        it "round-trips SpanShot through Seq conversions" $ do
            let err = mockEvent 5 "ERROR"
            let pre = [mockEvent 1 "pre1", mockEvent 2 "pre2", mockEvent 3 "pre3"]
            let post = [mockEvent 6 "post1", mockEvent 7 "post2"]
            let rules = [RegexRule "ERROR"]
            let time = mockTime 100
            let original = SpanShot err pre post rules time
            let (errOut, preSeq, postSeq) = spanShotToSeq original
            let reconstructed = spanShotFromSeq errOut preSeq postSeq rules time
            reconstructed `shouldBe` original

        it "handles empty pre and post windows" $ do
            let err = mockEvent 5 "ERROR"
            let rules = [RegexRule "ERROR"]
            let time = mockTime 10
            let shot = SpanShot err [] [] rules time
            let (errOut, preSeq, postSeq) = spanShotToSeq shot
            errOut `shouldBe` err
            Seq.null preSeq `shouldBe` True
            Seq.null postSeq `shouldBe` True

    describe "CompiledRule instances" $ do
        it "Show instance displays rule pattern without regex" $ do
            case mkCaptureOptions 5 5 10 [RegexRule "ERROR"] of
                Right opts -> do
                    case compiledRules opts of
                        (x : _) -> do
                            -- Show should include the pattern but not expose internal Regex
                            show x `shouldSatisfy` ("ERROR" `isInfixOf`)
                            show x `shouldSatisfy` ("CompiledRule" `isPrefixOf`)
                        [] -> fail "Expected non-empty compiled rules"
                Left err -> fail err

        it "Eq instance compares by original rule only" $ do
            -- Create two CaptureOptions with same rules - compiled regexes should be equal
            case (mkCaptureOptions 5 5 10 [RegexRule "ERROR"], mkCaptureOptions 10 10 20 [RegexRule "ERROR"]) of
                (Right opts1, Right opts2) -> do
                    case (compiledRules opts1, compiledRules opts2) of
                        (c1 : _, c2 : _) -> c1 `shouldBe` c2
                        _ -> fail "Expected non-empty compiled rules"
                _ -> fail "Failed to create options"

        it "Eq instance distinguishes different patterns" $ do
            case (mkCaptureOptions 5 5 10 [RegexRule "ERROR"], mkCaptureOptions 5 5 10 [RegexRule "FATAL"]) of
                (Right opts1, Right opts2) -> do
                    case (compiledRules opts1, compiledRules opts2) of
                        (c1 : _, c2 : _) -> (c1 == c2) `shouldBe` False
                        _ -> fail "Expected non-empty compiled rules"
                _ -> fail "Failed to create options"
