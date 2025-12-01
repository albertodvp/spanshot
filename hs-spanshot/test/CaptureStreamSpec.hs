{-# LANGUAGE OverloadedStrings #-}

module CaptureStreamSpec (captureStreamTests) where

import Data.Foldable (toList)
import Data.Maybe (isJust)
import Data.Sequence qualified as Seq
import Data.Time (NominalDiffTime)
import Test.Hspec (Spec, describe, it, shouldBe, shouldContain, shouldSatisfy)

import Capture (processEvent)
import Fixtures (mockEvent)
import Types (
    ActiveCapture (..),
    CaptureOptions,
    CaptureState (..),
    DetectionRule (..),
    SpanShot (..),
    initialCaptureState,
    mkCaptureOptions,
 )

{- | Explicit test constants for clarity and maintainability
NominalDiffTime uses seconds when constructed via Num instance
-}
testPreWindowSeconds :: NominalDiffTime
testPreWindowSeconds = 5

testPostWindowSeconds :: NominalDiffTime
testPostWindowSeconds = 5

testMinContextEvents :: Int
testMinContextEvents = 10

testDetectionRules :: [DetectionRule]
testDetectionRules = [RegexRule "ERROR"]

-- | Helper to create test CaptureOptions with explicit values
testCaptureOptions :: CaptureOptions
testCaptureOptions = case mkCaptureOptions testPreWindowSeconds testPostWindowSeconds testMinContextEvents testDetectionRules of
    Right opts -> opts
    Left err -> error $ "Invalid test options: " <> err

captureStreamTests :: Spec
captureStreamTests = do
    describe "Error detection and capture" $ do
        it "creates ActiveCapture when error detected" $ do
            let opts = testCaptureOptions
            let state = initialCaptureState
            let preEvents = [mockEvent 1 ("INFO"), mockEvent 2 ("INFO")]
            let stateWithPre = state{csPreWindow = Seq.fromList preEvents}
            let errorEvt = mockEvent 3 ("ERROR occurred")

            let (newState, emitted) = processEvent opts stateWithPre errorEvt

            case csActiveCapture newState of
                Nothing -> fail "Expected ActiveCapture but got Nothing"
                Just cap -> do
                    acErrorEvent cap `shouldBe` errorEvt
                    Seq.length (acPreWindowSnapshot cap) `shouldBe` 2
                    Seq.length (acPostEvents cap) `shouldBe` 0
            emitted `shouldBe` Nothing

        it "snapshot does not include error event itself" $ do
            let opts = testCaptureOptions
            let state = initialCaptureState
            let preEvents = [mockEvent 1 ("INFO"), mockEvent 2 ("INFO")]
            let stateWithPre = state{csPreWindow = Seq.fromList preEvents}
            let errorEvt = mockEvent 3 ("ERROR")

            let (newState, _) = processEvent opts stateWithPre errorEvt

            case csActiveCapture newState of
                Nothing -> fail "Expected ActiveCapture but got Nothing"
                Just cap -> Seq.length (acPreWindowSnapshot cap) `shouldBe` 2
            -- Error event is NOT in the snapshot (which was captured BEFORE the error)
            -- but IS added to the main pre-window buffer for future errors to reference
            csPreWindow newState `shouldSatisfy` (\w -> errorEvt `elem` toList w)

        it "does not create capture for non-error events" $ do
            let opts = testCaptureOptions
            let state = initialCaptureState
            -- Process multiple non-error events to verify state consistency
            let infoEvt1 = mockEvent 1 "INFO message 1"
            let infoEvt2 = mockEvent 2 "INFO message 2"
            let infoEvt3 = mockEvent 3 "DEBUG message"

            let (state1, emitted1) = processEvent opts state infoEvt1
            let (state2, emitted2) = processEvent opts state1 infoEvt2
            let (state3, emitted3) = processEvent opts state2 infoEvt3

            csActiveCapture state3 `shouldBe` Nothing
            emitted1 `shouldBe` Nothing
            emitted2 `shouldBe` Nothing
            emitted3 `shouldBe` Nothing
            -- Pre-window should contain all events
            Seq.length (csPreWindow state3) `shouldBe` 3

    describe "Post-window tracking" $ do
        it "adds events to active post-window" $ do
            let opts = testCaptureOptions
            let errorEvt = mockEvent 10 ("ERROR")
            let cap = ActiveCapture errorEvt [RegexRule "ERROR"] Seq.empty Seq.empty
            let state = initialCaptureState{csActiveCapture = Just cap}
            let postEvt = mockEvent 11 ("INFO after error")

            let (newState, emitted) = processEvent opts state postEvt

            case csActiveCapture newState of
                Nothing -> fail "Expected active capture to continue"
                Just updatedCap -> Seq.length (acPostEvents updatedCap) `shouldBe` 1
            emitted `shouldBe` Nothing

        it "emits SpanShot when post-window duration reached" $ do
            let opts = testCaptureOptions
            let errorEvt = mockEvent 10 ("ERROR")
            let preSnap = Seq.fromList [mockEvent 8 ("pre1"), mockEvent 9 ("pre2")]
            let post = Seq.fromList [mockEvent 11 ("post1"), mockEvent 12 ("post2")]
            let cap = ActiveCapture errorEvt [RegexRule "ERROR"] preSnap post
            let state = initialCaptureState{csActiveCapture = Just cap}
            let finalEvt = mockEvent 16 ("INFO")

            let (newState, emitted) = processEvent opts state finalEvt

            case emitted of
                Just spanshot -> do
                    errorEvent spanshot `shouldBe` errorEvt
                    length (preWindow spanshot) `shouldBe` 2
                    length (postWindow spanshot) `shouldBe` 2
                Nothing -> fail "Expected a SpanShot"
            csActiveCapture newState `shouldBe` Nothing

        it "continues tracking when post-window duration not reached" $ do
            let opts = testCaptureOptions
            let errorEvt = mockEvent 10 ("ERROR")
            let cap = ActiveCapture errorEvt [RegexRule "ERROR"] Seq.empty Seq.empty
            let state = initialCaptureState{csActiveCapture = Just cap}
            let earlyEvt = mockEvent 12 ("INFO")

            let (newState, emitted) = processEvent opts state earlyEvt

            emitted `shouldBe` Nothing
            case csActiveCapture newState of
                Nothing -> fail "Expected capture to continue"
                Just updatedCap -> do
                    Seq.length (acPostEvents updatedCap) `shouldBe` 1
                    toList (acPostEvents updatedCap) `shouldBe` [earlyEvt]

    describe "Single active capture policy" $ do
        it "second error goes into first error's post-window" $ do
            let opts = testCaptureOptions
            let error1 = mockEvent 10 ("ERROR first")
            let cap = ActiveCapture error1 [RegexRule "ERROR"] Seq.empty Seq.empty
            let state = initialCaptureState{csActiveCapture = Just cap}
            let error2 = mockEvent 12 ("ERROR second")

            let (newState, emitted) = processEvent opts state error2

            case csActiveCapture newState of
                Nothing -> fail "Expected active capture to continue"
                Just updatedCap -> do
                    acErrorEvent updatedCap `shouldBe` error1
                    toList (acPostEvents updatedCap) `shouldContain` [error2]
            emitted `shouldBe` Nothing

        it "creates new capture after previous completes" $ do
            let opts = testCaptureOptions
            let error1 = mockEvent 10 ("ERROR first")
            let cap = ActiveCapture error1 [RegexRule "ERROR"] Seq.empty Seq.empty
            let state = initialCaptureState{csActiveCapture = Just cap, csPreWindow = Seq.empty}
            let completionEvt = mockEvent 16 ("INFO")

            let (stateAfterCompletion, emitted1) = processEvent opts state completionEvt
            emitted1 `shouldSatisfy` isJust
            csActiveCapture stateAfterCompletion `shouldBe` Nothing

            let error2 = mockEvent 20 ("ERROR second")
            let (finalState, emitted2) = processEvent opts stateAfterCompletion error2

            csActiveCapture finalState `shouldSatisfy` isJust
            case csActiveCapture finalState of
                Nothing -> fail "Expected new capture"
                Just newCap -> acErrorEvent newCap `shouldBe` error2
            emitted2 `shouldBe` Nothing

    describe "Post-window boundary tests" $ do
        it "does not emit SpanShot just before post-window boundary" $ do
            let opts = testCaptureOptions -- postWindowDuration = 5
            let errorEvt = mockEvent 10 "ERROR"
            let cap = ActiveCapture errorEvt testDetectionRules Seq.empty Seq.empty
            let state = initialCaptureState{csActiveCapture = Just cap}
            -- Event at t=14 is only 4 seconds after error at t=10 (< 5s threshold)
            let earlyEvt = mockEvent 14 "INFO just before boundary"

            let (newState, emitted) = processEvent opts state earlyEvt

            emitted `shouldBe` Nothing
            csActiveCapture newState `shouldSatisfy` isJust

        it "emits SpanShot exactly at post-window boundary" $ do
            let opts = testCaptureOptions -- postWindowDuration = 5
            let errorEvt = mockEvent 10 "ERROR"
            let cap = ActiveCapture errorEvt testDetectionRules Seq.empty Seq.empty
            let state = initialCaptureState{csActiveCapture = Just cap}
            -- Event at t=15 is exactly 5 seconds after error at t=10 (= 5s threshold)
            let boundaryEvt = mockEvent 15 "INFO at exact boundary"

            let (newState, emitted) = processEvent opts state boundaryEvt

            emitted `shouldSatisfy` isJust
            csActiveCapture newState `shouldBe` Nothing

        it "emits SpanShot after post-window boundary" $ do
            let opts = testCaptureOptions -- postWindowDuration = 5
            let errorEvt = mockEvent 10 "ERROR"
            let cap = ActiveCapture errorEvt testDetectionRules Seq.empty Seq.empty
            let state = initialCaptureState{csActiveCapture = Just cap}
            -- Event at t=16 is 6 seconds after error at t=10 (> 5s threshold)
            let lateEvt = mockEvent 16 "INFO after boundary"

            let (newState, emitted) = processEvent opts state lateEvt

            emitted `shouldSatisfy` isJust
            csActiveCapture newState `shouldBe` Nothing
