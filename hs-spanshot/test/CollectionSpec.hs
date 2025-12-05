{-# LANGUAGE CPP #-}

module CollectionSpec (collectionTests) where

import Control.Concurrent (threadDelay)
import Control.Exception (SomeException, try)
import Data.Either (isLeft, isRight)
import Data.Text qualified as T
import Streaming.Prelude qualified as S
import System.IO (IOMode (WriteMode), hClose, hPutStrLn, openFile, stderr)
import System.IO.Temp (emptySystemTempFile)
import System.Process (createProcess, shell, waitForProcess)
import Test.Hspec (Spec, describe, expectationFailure, it, shouldBe, shouldSatisfy)

import Collect (collectFromFile)
import Types (
    CollectEvent (line, sessionOrderId, source),
    CollectOptions,
    maxPollIntervalMs,
    minPollIntervalMs,
    mkCollectOptions,
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
