module SessionStateSpec (sessionStateTests) where

import Data.Text qualified as T
import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy)

import Session.State (
    Session (..),
    addCapture,
    endSession,
    getCaptures,
    newSession,
 )

sessionStateTests :: Spec
sessionStateTests = do
    describe "newSession" $ do
        it "creates a session with a unique ID" $ do
            session <- newSession "/bin/sh"
            sessionId session `shouldSatisfy` (not . T.null)

        it "creates a session with the specified shell path" $ do
            session <- newSession "/bin/bash"
            sessionShellPath session `shouldBe` "/bin/bash"

        it "creates an active session" $ do
            session <- newSession "/bin/sh"
            sessionIsActive session `shouldBe` True

        it "creates a session with no captures" $ do
            session <- newSession "/bin/sh"
            sessionCaptureIds session `shouldBe` []

        it "generates different IDs for different sessions" $ do
            session1 <- newSession "/bin/sh"
            session2 <- newSession "/bin/sh"
            sessionId session1 `shouldSatisfy` (/= sessionId session2)

    describe "addCapture" $ do
        it "adds a capture ID to the session" $ do
            session <- newSession "/bin/sh"
            let updated = addCapture session "capture-123"
            getCaptures updated `shouldBe` ["capture-123"]

        it "adds multiple captures in order (most recent first)" $ do
            session <- newSession "/bin/sh"
            let s1 = addCapture session "capture-1"
            let s2 = addCapture s1 "capture-2"
            let s3 = addCapture s2 "capture-3"
            getCaptures s3 `shouldBe` ["capture-3", "capture-2", "capture-1"]

    describe "getCaptures" $ do
        it "returns empty list for new session" $ do
            session <- newSession "/bin/sh"
            getCaptures session `shouldBe` []

    describe "endSession" $ do
        it "marks session as inactive" $ do
            session <- newSession "/bin/sh"
            let ended = endSession session
            sessionIsActive ended `shouldBe` False

        it "preserves captures when ending session" $ do
            session <- newSession "/bin/sh"
            let withCapture = addCapture session "capture-123"
            let ended = endSession withCapture
            getCaptures ended `shouldBe` ["capture-123"]
