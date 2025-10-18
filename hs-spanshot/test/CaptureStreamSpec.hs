module CaptureStreamSpec (captureStreamTests) where

import Data.Foldable (toList)
import Data.Sequence qualified as Seq
import Data.Text qualified as T
import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy)

import Capture (processEvent)
import Fixtures (mockEvent)
import Types (
    ActiveCapture (acErrorEvent, acPostEvents, acPreWindowSnapshot),
    CaptureOptions,
    CaptureState (csActiveCapture, csPreWindow),
    CollectEvent,
    SpanShot,
    defaultCaptureOptions,
    initialCaptureState,
 )

captureStreamTests :: Spec
captureStreamTests = do
    describe "Error detection and capture" $ do
        it "creates ActiveCapture when error detected" $ do
            let opts = defaultCaptureOptions
            let state = initialCaptureState
            let preEvents = [mockEvent 1 (T.pack "INFO"), mockEvent 2 (T.pack "INFO")]
            let stateWithPre = state{csPreWindow = Seq.fromList preEvents}
            let errorEvt = mockEvent 3 (T.pack "ERROR occurred")

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
            let preEvents = [mockEvent 1 (T.pack "INFO"), mockEvent 2 (T.pack "INFO")]
            let stateWithPre = state{csPreWindow = Seq.fromList preEvents}
            let errorEvt = mockEvent 3 (T.pack "ERROR")

            let (newState, _) = processEvent opts stateWithPre errorEvt

            case csActiveCapture newState of
                Nothing -> fail "Expected ActiveCapture but got Nothing"
                Just cap -> Seq.length (acPreWindowSnapshot cap) `shouldBe` 2
            csPreWindow newState `shouldSatisfy` (\w -> errorEvt `elem` toList w)

        it "does not create capture for non-error events" $ do
            let opts = defaultCaptureOptions
            let state = initialCaptureState
            let infoEvt = mockEvent 1 (T.pack "INFO message")

            let (newState, emitted) = processEvent opts state infoEvt

            csActiveCapture newState `shouldBe` Nothing
            emitted `shouldBe` []
