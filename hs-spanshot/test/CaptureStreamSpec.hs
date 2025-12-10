{-# LANGUAGE OverloadedStrings #-}

module CaptureStreamSpec (captureStreamTests) where

import Data.Foldable (toList)
import Data.Functor.Identity (runIdentity)
import Data.Maybe (isJust, isNothing)
import Data.Sequence qualified as Seq
import Data.Time (NominalDiffTime)
import Streaming.Prelude qualified as S
import Test.Hspec (Spec, describe, it, shouldBe, shouldContain, shouldSatisfy)

import Capture (captureFromStream, flushPendingCaptures, processEvent)
import Fixtures (mockEvent, mockTime)
import Types (
    ActiveCapture (..),
    CaptureOptions,
    CaptureState (..),
    CollectEvent (line),
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

    describe "captureFromStream combinator" $ do
        it "produces no SpanShots from empty stream" $ do
            let opts = testCaptureOptions
            let events = [] :: [CollectEvent]
            let result = runIdentity $ S.toList_ $ captureFromStream opts (S.each events)
            result `shouldBe` []

        it "produces no SpanShots when no errors detected" $ do
            let opts = testCaptureOptions
            let events = [mockEvent 1 "INFO", mockEvent 2 "DEBUG", mockEvent 3 "INFO"]
            let result = runIdentity $ S.toList_ $ captureFromStream opts (S.each events)
            result `shouldBe` []

        it "produces SpanShot when error detected and post-window completes" $ do
            let opts = testCaptureOptions -- pre=5s, post=5s
            let events =
                    [ mockEvent 1 "INFO pre1"
                    , mockEvent 2 "INFO pre2"
                    , mockEvent 10 "ERROR detected"
                    , mockEvent 11 "INFO post1"
                    , mockEvent 12 "INFO post2"
                    , mockEvent 16 "INFO triggers emit" -- 6s after error
                    ]
            let result = runIdentity $ S.toList_ $ captureFromStream opts (S.each events)
            length result `shouldBe` 1
            case result of
                [shot] -> do
                    line (errorEvent shot) `shouldBe` "ERROR detected"
                    length (preWindow shot) `shouldBe` 2
                    length (postWindow shot) `shouldBe` 2
                _ -> fail "Expected exactly one SpanShot"

        it "drops incomplete capture when stream ends before post-window" $ do
            let opts = testCaptureOptions -- post=5s
            let events =
                    [ mockEvent 1 "INFO"
                    , mockEvent 10 "ERROR detected"
                    , mockEvent 12 "INFO" -- only 2s after error, not enough
                    ]
            let result = runIdentity $ S.toList_ $ captureFromStream opts (S.each events)
            result `shouldBe` []

        it "captures multiple errors with proper spacing" $ do
            let opts = testCaptureOptions -- pre=5s, post=5s
            let events =
                    [ mockEvent 1 "INFO"
                    , mockEvent 10 "ERROR first"
                    , mockEvent 11 "INFO"
                    , mockEvent 16 "INFO completes first" -- completes first error
                    , mockEvent 20 "ERROR second"
                    , mockEvent 21 "INFO"
                    , mockEvent 26 "INFO completes second" -- completes second error
                    ]
            let result = runIdentity $ S.toList_ $ captureFromStream opts (S.each events)
            length result `shouldBe` 2

        it "second error during active capture goes to post-window" $ do
            let opts = testCaptureOptions -- post=5s
            let events =
                    [ mockEvent 1 "INFO"
                    , mockEvent 10 "ERROR first"
                    , mockEvent 12 "ERROR second" -- 2s after first, goes to post-window
                    , mockEvent 16 "INFO" -- triggers emit
                    ]
            let result = runIdentity $ S.toList_ $ captureFromStream opts (S.each events)
            length result `shouldBe` 1
            case result of
                [shot] -> do
                    line (errorEvent shot) `shouldBe` "ERROR first"
                    -- second error should be in post-window
                    map line (postWindow shot) `shouldContain` ["ERROR second"]
                _ -> fail "Expected exactly one SpanShot"

    describe "flushPendingCaptures" $ do
        it "returns Nothing when no active capture" $ do
            let state = initialCaptureState
            let flushTime = mockTime 100
            flushPendingCaptures state flushTime `shouldSatisfy` isNothing

        it "emits SpanShot with collected post-window events when active capture exists" $ do
            let errorEvt = mockEvent 10 "ERROR detected"
            let preSnap = Seq.fromList [mockEvent 8 "pre1", mockEvent 9 "pre2"]
            let postEvts = Seq.fromList [mockEvent 11 "post1", mockEvent 12 "post2"]
            let cap =
                    ActiveCapture
                        { acErrorEvent = errorEvt
                        , acDetectedBy = [RegexRule "ERROR"]
                        , acPreWindowSnapshot = preSnap
                        , acPostEvents = postEvts
                        }
            let state = initialCaptureState{csActiveCapture = Just cap}
            let flushTime = mockTime 100

            case flushPendingCaptures state flushTime of
                Nothing -> fail "Expected SpanShot from flush"
                Just shot -> do
                    errorEvent shot `shouldBe` errorEvt
                    length (preWindow shot) `shouldBe` 2
                    length (postWindow shot) `shouldBe` 2
                    capturedAtUtc shot `shouldBe` flushTime

        it "emits SpanShot with empty post-window if no post events collected" $ do
            let errorEvt = mockEvent 10 "ERROR detected"
            let preSnap = Seq.fromList [mockEvent 8 "pre1"]
            let cap =
                    ActiveCapture
                        { acErrorEvent = errorEvt
                        , acDetectedBy = [RegexRule "ERROR"]
                        , acPreWindowSnapshot = preSnap
                        , acPostEvents = Seq.empty
                        }
            let state = initialCaptureState{csActiveCapture = Just cap}
            let flushTime = mockTime 100

            case flushPendingCaptures state flushTime of
                Nothing -> fail "Expected SpanShot from flush"
                Just shot -> do
                    errorEvent shot `shouldBe` errorEvt
                    length (preWindow shot) `shouldBe` 1
                    length (postWindow shot) `shouldBe` 0

        it "preserves detection rules in flushed SpanShot" $ do
            let errorEvt = mockEvent 10 "ERROR FATAL"
            let rules = [RegexRule "ERROR", RegexRule "FATAL"]
            let cap =
                    ActiveCapture
                        { acErrorEvent = errorEvt
                        , acDetectedBy = rules
                        , acPreWindowSnapshot = Seq.empty
                        , acPostEvents = Seq.empty
                        }
            let state = initialCaptureState{csActiveCapture = Just cap}
            let flushTime = mockTime 100

            case flushPendingCaptures state flushTime of
                Nothing -> fail "Expected SpanShot from flush"
                Just shot -> detectedBy shot `shouldBe` rules
