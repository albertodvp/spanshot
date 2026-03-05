{-# LANGUAGE OverloadedStrings #-}

module CaptureStreamSpec (captureStreamTests) where

import Data.Foldable (toList)
import Data.Functor.Identity (Identity (runIdentity))
import Data.Maybe (isJust)
import Data.Sequence qualified as Seq
import Data.Time (NominalDiffTime)
import Streaming (Of ((:>)))
import Streaming.Prelude qualified as S
import Test.Hspec (Spec, describe, it, shouldBe, shouldContain, shouldSatisfy)

import Capture (captureFromStream, finalizeCapture, processEvent)
import Fixtures (mockEvent, mockTime)
import Types (
    ActiveCapture (..),
    CaptureOptions,
    CaptureState (..),
    CollectEvent,
    DetectionRule (..),
    SpanShot (..),
    defaultMaxPostWindowEvents,
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
testCaptureOptions = case mkCaptureOptions testPreWindowSeconds testPostWindowSeconds testMinContextEvents defaultMaxPostWindowEvents testDetectionRules of
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

    -- T005: captureFromStream basic capture test
    describe "captureFromStream" $ do
        it "transforms stream of events into stream of SpanShots" $ do
            let opts = testCaptureOptions
            -- Create a stream with pre-context, error, and post-context events
            let events =
                    [ mockEvent 1 "INFO startup"
                    , mockEvent 2 "INFO connecting"
                    , mockEvent 3 "INFO connected"
                    , mockEvent 10 "ERROR connection failed"
                    , mockEvent 11 "INFO retrying"
                    , mockEvent 12 "INFO retry 1"
                    , mockEvent 16 "INFO recovered" -- triggers emission (6s > 5s post-window)
                    ]
            let inputStream = S.each events
            let outputStream = captureFromStream opts inputStream
            let (spanshots :> _) = runIdentity $ S.toList outputStream

            length spanshots `shouldBe` 1
            let shot = head spanshots
            errorEvent shot `shouldBe` mockEvent 10 "ERROR connection failed"
            -- Pre-window should contain events within 5s before error
            length (preWindow shot) `shouldBe` 3
            -- Post-window should contain events collected before emission trigger
            -- t=16 triggers emission but is NOT included (it's outside the window)
            length (postWindow shot) `shouldBe` 2

        -- T006: captureFromStream finalization test (stream ends during capture)
        it "emits in-flight capture when stream ends" $ do
            let opts = testCaptureOptions
            -- Stream ends before post-window completes
            let events =
                    [ mockEvent 1 "INFO startup"
                    , mockEvent 2 "INFO connecting"
                    , mockEvent 10 "ERROR failed"
                    , mockEvent 11 "INFO partial post" -- Only 1s into post-window
                    ]
            let inputStream = S.each events
            let outputStream = captureFromStream opts inputStream
            let (spanshots :> _) = runIdentity $ S.toList outputStream

            length spanshots `shouldBe` 1
            let shot = head spanshots
            errorEvent shot `shouldBe` mockEvent 10 "ERROR failed"
            -- Partial post-window (stream ended early)
            length (postWindow shot) `shouldBe` 1

        -- T007: captureFromStream with no matching events
        it "returns empty stream when no errors match" $ do
            let opts = testCaptureOptions
            let events =
                    [ mockEvent 1 "INFO startup"
                    , mockEvent 2 "INFO connecting"
                    , mockEvent 3 "INFO connected"
                    , mockEvent 4 "DEBUG trace"
                    ]
            let inputStream = S.each events
            let outputStream = captureFromStream opts inputStream
            let (spanshots :> _) = runIdentity $ S.toList outputStream

            spanshots `shouldBe` []

        -- T009: empty input stream test
        it "handles empty input stream" $ do
            let opts = testCaptureOptions
            let inputStream = S.each ([] :: [CollectEvent])
            let outputStream = captureFromStream opts inputStream
            let (spanshots :> _) = runIdentity $ S.toList outputStream

            spanshots `shouldBe` []

        -- T010: rapid error bursts (single-active-capture policy)
        it "only captures first error when multiple errors in rapid succession" $ do
            let opts = testCaptureOptions
            let events =
                    [ mockEvent 1 "INFO startup"
                    , mockEvent 10 "ERROR first"
                    , mockEvent 11 "ERROR second" -- Goes into post-window of first
                    , mockEvent 12 "ERROR third" -- Also goes into post-window
                    , mockEvent 16 "INFO triggers emission"
                    ]
            let inputStream = S.each events
            let outputStream = captureFromStream opts inputStream
            let (spanshots :> _) = runIdentity $ S.toList outputStream

            length spanshots `shouldBe` 1
            let shot = head spanshots
            errorEvent shot `shouldBe` mockEvent 10 "ERROR first"
            -- Both subsequent errors should be in post-window
            postWindow shot `shouldContain` [mockEvent 11 "ERROR second"]
            postWindow shot `shouldContain` [mockEvent 12 "ERROR third"]

        -- T011: sparse pre-window (fewer events than duration)
        it "captures available context when pre-window has fewer events" $ do
            let opts = testCaptureOptions -- 5 second pre-window
            -- Error occurs early with minimal pre-context
            let events =
                    [ mockEvent 9 "INFO single pre-context" -- Only 1s before error
                    , mockEvent 10 "ERROR early error"
                    , mockEvent 16 "INFO triggers emission"
                    ]
            let inputStream = S.each events
            let outputStream = captureFromStream opts inputStream
            let (spanshots :> _) = runIdentity $ S.toList outputStream

            length spanshots `shouldBe` 1
            let shot = head spanshots
            -- Only 1 pre-context event available
            length (preWindow shot) `shouldBe` 1
            preWindow shot `shouldBe` [mockEvent 9 "INFO single pre-context"]

        it "preserves stream return value" $ do
            let opts = testCaptureOptions
            let events = [mockEvent 1 "INFO only"]
            let inputStream = S.each events >> return (42 :: Int)
            let outputStream = captureFromStream opts inputStream
            let (_ :> result) = runIdentity $ S.toList outputStream

            result `shouldBe` 42

    -- T008: finalizeCapture test
    describe "finalizeCapture" $ do
        it "converts ActiveCapture to SpanShot with current timestamp" $ do
            let errorEvt = mockEvent 10 "ERROR test"
            let preSnap = Seq.fromList [mockEvent 8 "pre1", mockEvent 9 "pre2"]
            let postEvts = Seq.fromList [mockEvent 11 "post1"]
            let rules = [RegexRule "ERROR"]
            let cap =
                    ActiveCapture
                        { acErrorEvent = errorEvt
                        , acDetectedBy = rules
                        , acPreWindowSnapshot = preSnap
                        , acPostEvents = postEvts
                        }
            let currentTime = mockTime 12

            let shot = finalizeCapture cap currentTime

            errorEvent shot `shouldBe` errorEvt
            preWindow shot `shouldBe` [mockEvent 8 "pre1", mockEvent 9 "pre2"]
            postWindow shot `shouldBe` [mockEvent 11 "post1"]
            detectedBy shot `shouldBe` rules
            capturedAtUtc shot `shouldBe` currentTime

        it "handles empty post-window" $ do
            let errorEvt = mockEvent 10 "ERROR test"
            let preSnap = Seq.fromList [mockEvent 9 "pre"]
            let cap =
                    ActiveCapture
                        { acErrorEvent = errorEvt
                        , acDetectedBy = [RegexRule "ERROR"]
                        , acPreWindowSnapshot = preSnap
                        , acPostEvents = Seq.empty
                        }
            let currentTime = mockTime 10

            let shot = finalizeCapture cap currentTime

            postWindow shot `shouldBe` []
            preWindow shot `shouldBe` [mockEvent 9 "pre"]
