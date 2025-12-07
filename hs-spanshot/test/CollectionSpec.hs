{-# LANGUAGE CPP #-}

module CollectionSpec (collectionTests) where

import Control.Concurrent (threadDelay)
import Control.Exception (SomeException, try)
import Data.Either (isLeft, isRight)
import Data.Text qualified as T
import Streaming.Prelude qualified as S
import System.IO (IOMode (WriteMode), hClose, hPutStrLn, openFile, stderr)
import System.IO.Temp (emptySystemTempFile, withSystemTempFile)
import System.Process (createProcess, shell, waitForProcess)
import Test.Hspec (Spec, describe, expectationFailure, it, shouldBe, shouldSatisfy)

import Collect (collectFromFile, collectFromFileWithCleanup)
import Types (
    CollectEvent (line, sessionOrderId, source),
    CollectOptions,
    defaultCollectOptions,
    maxPollIntervalMs,
    minPollIntervalMs,
    mkCollectOptions,
    pollIntervalMs,
 )

testOptions :: CollectOptions
testOptions = case mkCollectOptions 50 of
    Right opts -> opts
    Left err -> error $ "Invalid test options: " ++ err

collectionTests :: Spec
collectionTests = do
    describe "collectFromFile" $ do
        it "reads all lines from a static file" $ do
            events <- S.toList_ $ S.take 5 $ collectFromFile testOptions "test/fixtures/small.log"
            length events `shouldBe` 5
            map sessionOrderId events `shouldBe` [0, 1, 2, 3, 4]
            map line events
                `shouldBe` [ T.pack "first line"
                           , T.pack "second line"
                           , T.pack "third line"
                           , T.pack "fourth line"
                           , T.pack "fifth line"
                           ]
            all (\e -> source e == T.pack "test/fixtures/small.log") events `shouldBe` True

        it "fails with clear error when file does not exist" $ do
            result <- try @SomeException $ S.toList_ $ S.take 1 $ collectFromFile testOptions "test/fixtures/nonexistent.log"
            case result of
                Left _ -> pure ()
                Right _ -> expectationFailure "Expected an exception but got success"

    describe "collectFromFileWithCleanup" $ do
        it "reads lines and properly cleans up resources" $ do
            withSystemTempFile "cleanup_test.log" $ \path handle -> do
                hPutStrLn handle "line 1"
                hPutStrLn handle "line 2"
                hPutStrLn handle "line 3"
                hClose handle

                events <- collectFromFileWithCleanup testOptions path $ \stream ->
                    S.toList_ $ S.take 3 stream

                length events `shouldBe` 3
                map line events `shouldBe` [T.pack "line 1", T.pack "line 2", T.pack "line 3"]

        it "cleans up even when action throws" $ do
            withSystemTempFile "cleanup_error_test.log" $ \path handle -> do
                hPutStrLn handle "test line"
                hClose handle

                -- The action throws but bracket should still cleanup
                result <- try @SomeException $ collectFromFileWithCleanup testOptions path $ \_ ->
                    error "Intentional test error" :: IO ()

                -- We expect an exception
                result `shouldSatisfy` isLeft

        it "returns result from action callback" $ do
            withSystemTempFile "callback_test.log" $ \path handle -> do
                hPutStrLn handle "line 1"
                hPutStrLn handle "line 2"
                hClose handle

                -- The callback can return any type
                count <- collectFromFileWithCleanup testOptions path $ \stream ->
                    S.length_ $ S.take 2 stream

                count `shouldBe` 2

    describe "CollectOptions validation" $ do
        it "accepts valid poll interval" $ do
            let result = mkCollectOptions 100
            result `shouldSatisfy` isRight

        it "rejects poll interval less than 10ms" $ do
            let result = mkCollectOptions 5
            result `shouldSatisfy` isLeft

        it "rejects negative poll interval" $ do
            let result = mkCollectOptions (-1)
            result `shouldSatisfy` isLeft

        it "rejects poll interval greater than 60 seconds" $ do
            let result = mkCollectOptions (maxPollIntervalMs + 1)
            result `shouldSatisfy` isLeft

        it "accepts boundary values" $ do
            let result1 = mkCollectOptions minPollIntervalMs
            let result2 = mkCollectOptions maxPollIntervalMs
            result1 `shouldSatisfy` isRight
            result2 `shouldSatisfy` isRight

    describe "defaultCollectOptions" $ do
        it "has sensible default poll interval" $ do
            let opts = defaultCollectOptions
            pollIntervalMs opts `shouldSatisfy` (>= minPollIntervalMs)
            pollIntervalMs opts `shouldSatisfy` (<= maxPollIntervalMs)

        it "default poll interval is 150ms" $ do
            pollIntervalMs defaultCollectOptions `shouldBe` 150

#ifndef mingw32_HOST_OS
        -- NOTE: This test is skipped on Windows due to file locking limitations.
        -- On Windows, when collectFromFile opens a file in ReadMode, it prevents
        -- other processes from opening the same file in AppendMode (strict file locking).
        -- This is a known limitation documented in README.md.
        -- Future work: Implement proper file sharing using Win32 API (fILE_SHARE_READ | fILE_SHARE_WRITE).
        it "streams appended lines (polling behavior)" $ do
            tempFile <- emptySystemTempFile "stream_test.log"
            hPutStrLn stderr $ "\nTemp file: " <> tempFile

            handle <- openFile tempFile WriteMode
            hPutStrLn handle "line 1"
            hPutStrLn handle "line 2"
            hClose handle

            let writerScript =
                    "sleep 0.2 && echo line 3 >> "
                        ++ tempFile
                        ++ " && echo line 4 >> "
                        ++ tempFile
                        ++ " && echo line 5 >> "
                        ++ tempFile

            (_, _, _, writerHandle) <- createProcess (shell writerScript)

            threadDelay 10000

            events <- S.toList_ $ S.take 5 $ collectFromFile testOptions tempFile

            _ <- waitForProcess writerHandle
            length events `shouldBe` 5
            map sessionOrderId events `shouldBe` [0, 1, 2, 3, 4]
            map line events `shouldBe` [T.pack "line 1", T.pack "line 2", T.pack "line 3", T.pack "line 4", T.pack "line 5"]
#endif
