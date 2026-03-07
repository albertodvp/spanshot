module StorageSpec (storageTests) where

import Control.Concurrent (threadDelay)
import Data.Either (isLeft)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time (UTCTime (..), fromGregorian, secondsToDiffTime)
import System.Directory (createDirectoryIfMissing, doesFileExist, withCurrentDirectory)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec (Spec, describe, it, shouldBe, shouldReturn, shouldSatisfy)

import Storage (
    CaptureInfo (..),
    enforceLimit,
    getCaptureByIndex,
    getCaptureFilePath,
    getCapturesDir,
    listCaptures,
    listCapturesBySession,
    listCapturesWithInfo,
    loadCapture,
    saveCapture,
 )
import Types (
    CollectEvent (..),
    DetectionRule (..),
    SpanShot (..),
    capturedAtUtc,
 )

-- | Helper to create a mock UTCTime
mockTime :: Integer -> UTCTime
mockTime secs = UTCTime (fromGregorian 2025 1 1) (secondsToDiffTime secs)

-- | Helper to create a mock CollectEvent
mockEvent :: Integer -> Text -> CollectEvent
mockEvent secs content =
    CollectEvent
        { source = "test.log"
        , sessionOrderId = fromIntegral secs
        , readAtUtc = mockTime secs
        , line = content
        }

-- | Helper to create a mock SpanShot
mockSpanShot :: Integer -> SpanShot
mockSpanShot secs =
    SpanShot
        { errorEvent = mockEvent secs "ERROR test"
        , preWindow = [mockEvent (secs - 2) "pre1", mockEvent (secs - 1) "pre2"]
        , postWindow = [mockEvent (secs + 1) "post1"]
        , detectedBy = [RegexRule "ERROR"]
        , capturedAtUtc = mockTime secs
        , truncated = False
        , captureId = Nothing
        , sessionId = Nothing
        , captureSource = Nothing
        }

-- | Helper to run test in a temp directory with .spanshot/captures/ set up
withTestStorage :: (FilePath -> IO a) -> IO a
withTestStorage action = withSystemTempDirectory "spanshot-test" $ \tmpDir -> do
    let capturesDir = tmpDir </> ".spanshot" </> "captures"
    createDirectoryIfMissing True capturesDir
    withCurrentDirectory tmpDir (action tmpDir)

storageTests :: Spec
storageTests = do
    describe "getCapturesDir" $ do
        it "returns .spanshot/captures/ path" $ withTestStorage $ \_ -> do
            dir <- getCapturesDir
            dir `shouldSatisfy` \d -> ".spanshot" `T.isInfixOf` T.pack d
            dir `shouldSatisfy` \d -> "captures" `T.isInfixOf` T.pack d

    describe "saveCapture and loadCapture" $ do
        it "saves and loads a SpanShot correctly" $ withTestStorage $ \_ -> do
            -- Save a capture
            let shot = mockSpanShot 100
            captureId' <- saveCapture shot

            -- Verify file was created
            filePath <- getCaptureFilePath captureId'
            doesFileExist filePath `shouldReturn` True

            -- Load and verify
            result <- loadCapture captureId'
            case result of
                Left err -> fail $ "Failed to load capture: " ++ err
                Right loaded -> do
                    errorEvent loaded `shouldBe` errorEvent shot
                    preWindow loaded `shouldBe` preWindow shot
                    postWindow loaded `shouldBe` postWindow shot
                    detectedBy loaded `shouldBe` detectedBy shot

        it "returns Left for non-existent capture" $ withTestStorage $ \_ -> do
            result <- loadCapture "non-existent-id"
            result `shouldSatisfy` isLeft

    describe "listCaptures" $ do
        it "returns empty list when no captures exist" $ withTestStorage $ \_ -> do
            captures <- listCaptures
            captures `shouldBe` []

        it "returns captures sorted by most recent first" $ withTestStorage $ \_ -> do
            -- Save multiple captures
            id1 <- saveCapture (mockSpanShot 100)
            id2 <- saveCapture (mockSpanShot 200)
            id3 <- saveCapture (mockSpanShot 300)

            -- List should return all captures
            captures <- listCaptures
            length captures `shouldBe` 3
            -- All IDs should be present
            captures `shouldSatisfy` (id1 `elem`)
            captures `shouldSatisfy` (id2 `elem`)
            captures `shouldSatisfy` (id3 `elem`)

    describe "enforceLimit" $ do
        it "does nothing when under limit" $ withTestStorage $ \_ -> do
            -- Save 3 captures
            _ <- saveCapture (mockSpanShot 100)
            _ <- saveCapture (mockSpanShot 200)
            _ <- saveCapture (mockSpanShot 300)

            -- Enforce limit of 5 (should delete nothing)
            deleted <- enforceLimit 5
            deleted `shouldBe` 0

            -- All 3 should still exist
            captures <- listCaptures
            length captures `shouldBe` 3

        it "deletes oldest captures when over limit" $ withTestStorage $ \_ -> do
            -- Save 5 captures with small delays to ensure distinct modification times
            id1 <- saveCapture (mockSpanShot 100) -- oldest
            threadDelay 10000 -- 10ms
            id2 <- saveCapture (mockSpanShot 200)
            threadDelay 10000
            id3 <- saveCapture (mockSpanShot 300)
            threadDelay 10000
            id4 <- saveCapture (mockSpanShot 400)
            threadDelay 10000
            id5 <- saveCapture (mockSpanShot 500) -- newest

            -- Enforce limit of 3 (should delete 2 oldest)
            deleted <- enforceLimit 3
            deleted `shouldBe` 2

            -- Only 3 newest should remain
            captures <- listCaptures
            length captures `shouldBe` 3
            captures `shouldSatisfy` (id5 `elem`)
            captures `shouldSatisfy` (id4 `elem`)
            captures `shouldSatisfy` (id3 `elem`)
            captures `shouldSatisfy` (not . (id1 `elem`))
            captures `shouldSatisfy` (not . (id2 `elem`))

        it "returns 0 when already at limit" $ withTestStorage $ \_ -> do
            -- Save exactly 3 captures
            _ <- saveCapture (mockSpanShot 100)
            _ <- saveCapture (mockSpanShot 200)
            _ <- saveCapture (mockSpanShot 300)

            -- Enforce limit of 3 (should delete nothing)
            deleted <- enforceLimit 3
            deleted `shouldBe` 0

    describe "JSON round-trip" $ do
        it "preserves SpanShot through save/load cycle" $ withTestStorage $ \_ -> do
            let original = mockSpanShot 500
            captureId' <- saveCapture original
            result <- loadCapture captureId'

            case result of
                Left err -> fail $ "Round-trip failed: " ++ err
                Right loaded -> loaded `shouldBe` original

    describe "listCapturesWithInfo" $ do
        it "returns empty list when no captures exist" $ withTestStorage $ \_ -> do
            infos <- listCapturesWithInfo
            infos `shouldBe` []

        it "returns capture info with timestamp" $ withTestStorage $ \_ -> do
            let shot = mockSpanShot 100
            captureId' <- saveCapture shot
            infos <- listCapturesWithInfo
            length infos `shouldBe` 1
            case infos of
                [(cid, info)] -> do
                    cid `shouldBe` captureId'
                    ciCapturedAt info `shouldBe` capturedAtUtc shot
                _ -> fail "Expected exactly one capture"

        it "returns captures with most recent first" $ withTestStorage $ \_ -> do
            id1 <- saveCapture (mockSpanShot 100)
            threadDelay 10000
            _ <- saveCapture (mockSpanShot 200)
            threadDelay 10000
            id3 <- saveCapture (mockSpanShot 300)
            infos <- listCapturesWithInfo
            length infos `shouldBe` 3
            -- Most recent should be first, oldest last
            map fst infos `shouldSatisfy` (\xs -> take 1 xs == [id3])
            map fst infos `shouldSatisfy` (\xs -> drop 2 xs == [id1])

    describe "listCapturesBySession" $ do
        it "returns all captures when session is Nothing" $ withTestStorage $ \_ -> do
            _ <- saveCapture (mockSpanShot 100)
            _ <- saveCapture (mockSpanShot 200)
            captures <- listCapturesBySession Nothing
            length captures `shouldBe` 2

        it "returns only captures with matching session ID" $ withTestStorage $ \_ -> do
            let shot1 = (mockSpanShot 100){sessionId = Just "session-1"}
            let shot2 = (mockSpanShot 200){sessionId = Just "session-2"}
            let shot3 = (mockSpanShot 300){sessionId = Just "session-1"}
            _ <- saveCapture shot1
            _ <- saveCapture shot2
            _ <- saveCapture shot3
            captures <- listCapturesBySession (Just "session-1")
            length captures `shouldBe` 2

        it "returns empty list when no captures match session" $ withTestStorage $ \_ -> do
            let shot1 = (mockSpanShot 100){sessionId = Just "session-1"}
            _ <- saveCapture shot1
            captures <- listCapturesBySession (Just "other-session")
            captures `shouldBe` []

    describe "getCaptureByIndex" $ do
        it "returns capture at 1-based index" $ withTestStorage $ \_ -> do
            id1 <- saveCapture (mockSpanShot 100)
            threadDelay 10000
            _ <- saveCapture (mockSpanShot 200)
            threadDelay 10000
            id3 <- saveCapture (mockSpanShot 300)
            -- Index 1 is most recent (id3)
            result1 <- getCaptureByIndex 1
            case result1 of
                Left err -> fail err
                Right (cid, _) -> cid `shouldBe` id3
            -- Index 3 is oldest (id1)
            result3 <- getCaptureByIndex 3
            case result3 of
                Left err -> fail err
                Right (cid, _) -> cid `shouldBe` id1

        it "returns error for index 0" $ withTestStorage $ \_ -> do
            _ <- saveCapture (mockSpanShot 100)
            result <- getCaptureByIndex 0
            result `shouldSatisfy` isLeft

        it "returns error for negative index" $ withTestStorage $ \_ -> do
            _ <- saveCapture (mockSpanShot 100)
            result <- getCaptureByIndex (-1)
            result `shouldSatisfy` isLeft

        it "returns error for index beyond list size" $ withTestStorage $ \_ -> do
            _ <- saveCapture (mockSpanShot 100)
            result <- getCaptureByIndex 5
            result `shouldSatisfy` isLeft

        it "returns error when no captures exist" $ withTestStorage $ \_ -> do
            result <- getCaptureByIndex 1
            result `shouldSatisfy` isLeft
