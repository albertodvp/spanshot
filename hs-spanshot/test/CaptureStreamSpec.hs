{-# LANGUAGE OverloadedStrings #-}

module CaptureStreamSpec (captureStreamTests) where

import Data.Foldable (toList)
import Data.Maybe (isJust)
import Data.Sequence qualified as Seq
import Test.Hspec (Spec, describe, expectationFailure, it, shouldBe, shouldContain, shouldSatisfy)

import Capture (processEvent)
import Fixtures (mockEvent)
import Types (
    ActiveCapture (..),
    CaptureOptions (minContextEvents, preWindowDuration),
    CaptureState (..),
    DetectionRule (..),
    SpanShot (..),
    defaultCaptureOptions,
    initialCaptureState,
    mkCaptureOptions,
 )

captureStreamTests :: Spec
captureStreamTests = do
    describe "Error detection and capture" $ do
        it "creates ActiveCapture when error detected" $ do
            let opts = defaultCaptureOptions
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
            emitted `shouldBe` []

        it "snapshot does not include error event itself" $ do
            let opts = defaultCaptureOptions
            let state = initialCaptureState
            let preEvents = [mockEvent 1 ("INFO"), mockEvent 2 ("INFO")]
            let stateWithPre = state{csPreWindow = Seq.fromList preEvents}
            let errorEvt = mockEvent 3 ("ERROR")

            let (newState, _) = processEvent opts stateWithPre errorEvt

            case csActiveCapture newState of
                Nothing -> fail "Expected ActiveCapture but got Nothing"
                Just cap -> Seq.length (acPreWindowSnapshot cap) `shouldBe` 2
            csPreWindow newState `shouldSatisfy` (\w -> errorEvt `elem` toList w)

        it "does not create capture for non-error events" $ do
            let opts = defaultCaptureOptions
            let state = initialCaptureState
            let infoEvt = mockEvent 1 ("INFO message")

            let (newState, emitted) = processEvent opts state infoEvt

            csActiveCapture newState `shouldBe` Nothing
            emitted `shouldBe` []

    describe "Post-window tracking" $ do
        it "adds events to active post-window" $ do
            let opts = defaultCaptureOptions
            let errorEvt = mockEvent 10 ("ERROR")
            let cap = ActiveCapture errorEvt [RegexRule "ERROR"] Seq.empty Seq.empty
            let state = initialCaptureState{csActiveCapture = Just cap}
            let postEvt = mockEvent 11 ("INFO after error")

            let (newState, emitted) = processEvent opts state postEvt

            case csActiveCapture newState of
                Nothing -> fail "Expected active capture to continue"
                Just updatedCap -> Seq.length (acPostEvents updatedCap) `shouldBe` 1
            emitted `shouldBe` []

        it "emits SpanShot when post-window duration reached" $ do
            opts <- case mkCaptureOptions (preWindowDuration defaultCaptureOptions) 5 (minContextEvents defaultCaptureOptions) [RegexRule "ERROR"] of
                Right o -> pure o
                Left err -> fail $ "Invalid options: " <> err
            let errorEvt = mockEvent 10 ("ERROR")
            let preSnap = Seq.fromList [mockEvent 8 ("pre1"), mockEvent 9 ("pre2")]
            let post = Seq.fromList [mockEvent 11 ("post1"), mockEvent 12 ("post2")]
            let cap = ActiveCapture errorEvt [RegexRule "ERROR"] preSnap post
            let state = initialCaptureState{csActiveCapture = Just cap}
            let finalEvt = mockEvent 16 ("INFO")

            let (newState, emitted) = processEvent opts state finalEvt

            case emitted of
                [spanshot] -> do
                    errorEvent spanshot `shouldBe` errorEvt
                    length (preWindow spanshot) `shouldBe` 2
                    length (postWindow spanshot) `shouldBe` 2
                _ -> fail "Expected one element"
            csActiveCapture newState `shouldBe` Nothing

        it "continues tracking when post-window duration not reached" $ do
            opts <- case mkCaptureOptions (preWindowDuration defaultCaptureOptions) 5 (minContextEvents defaultCaptureOptions) [RegexRule "ERROR"] of
                Right o -> pure o
                Left err -> fail $ "Invalid options: " <> err
            let errorEvt = mockEvent 10 ("ERROR")
            let cap = ActiveCapture errorEvt [RegexRule "ERROR"] Seq.empty Seq.empty
            let state = initialCaptureState{csActiveCapture = Just cap}
            let earlyEvt = mockEvent 12 ("INFO")

            let (newState, emitted) = processEvent opts state earlyEvt

            emitted `shouldBe` []
            case csActiveCapture newState of
                Nothing -> fail "Expected capture to continue"
                Just updatedCap -> do
                    Seq.length (acPostEvents updatedCap) `shouldBe` 1
                    toList (acPostEvents updatedCap) `shouldBe` [earlyEvt]

    describe "Single active capture policy" $ do
        it "second error goes into first error's post-window" $ do
            opts <- case mkCaptureOptions (preWindowDuration defaultCaptureOptions) 5 (minContextEvents defaultCaptureOptions) [RegexRule "ERROR"] of
                Right o -> pure o
                Left err -> fail $ "Invalid options: " <> err
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
            emitted `shouldBe` []

        it "creates new capture after previous completes" $ do
            opts <- case mkCaptureOptions (preWindowDuration defaultCaptureOptions) 5 (minContextEvents defaultCaptureOptions) [RegexRule "ERROR"] of
                Right o -> pure o
                Left err -> fail $ "Invalid options: " <> err
            let error1 = mockEvent 10 ("ERROR first")
            let cap = ActiveCapture error1 [RegexRule "ERROR"] Seq.empty Seq.empty
            let state = initialCaptureState{csActiveCapture = Just cap, csPreWindow = Seq.empty}
            let completionEvt = mockEvent 16 ("INFO")

            let (stateAfterCompletion, emitted1) = processEvent opts state completionEvt
            length emitted1 `shouldBe` 1
            csActiveCapture stateAfterCompletion `shouldBe` Nothing

            let error2 = mockEvent 20 ("ERROR second")
            let (finalState, emitted2) = processEvent opts stateAfterCompletion error2

            csActiveCapture finalState `shouldSatisfy` isJust
            case csActiveCapture finalState of
                Nothing -> fail "Expected new capture"
                Just newCap -> acErrorEvent newCap `shouldBe` error2
            emitted2 `shouldBe` []
