{-# LANGUAGE CPP #-}

module CollectionSpec (collectionTests) where

import Control.Concurrent (threadDelay)
import Control.Exception (SomeException, try)
import Data.ByteString qualified as BS
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

        it "propagates exceptions from action callback" $ do
            withSystemTempFile "cleanup_error_test.log" $ \path handle -> do
                hPutStrLn handle "test line"
                hClose handle

                -- The action throws and the exception should propagate
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

    describe "UTF-8 handling" $ do
        it "replaces invalid UTF-8 bytes with replacement character" $ do
            withSystemTempFile "utf8_invalid.log" $ \path handle -> do
                -- Write "hello" + invalid byte 0xFF + "world" + newline
                -- 0xFF is never valid in UTF-8
                BS.hPut handle (BS.pack [0x68, 0x65, 0x6C, 0x6C, 0x6F, 0xFF, 0x77, 0x6F, 0x72, 0x6C, 0x64, 0x0A])
                hClose handle

                events <- S.toList_ $ S.take 1 $ collectFromFile testOptions path
                case events of
                    [event] -> line event `shouldBe` T.pack "hello\xFFFDworld"
                    _ -> expectationFailure "Expected exactly one event"

        it "handles truncated multi-byte UTF-8 sequences" $ do
            withSystemTempFile "utf8_truncated.log" $ \path handle -> do
                -- Write "test" + start of 3-byte sequence (0xE2) without continuation bytes + newline
                -- 0xE2 expects two more continuation bytes (0x80-0xBF)
                BS.hPut handle (BS.pack [0x74, 0x65, 0x73, 0x74, 0xE2, 0x0A])
                hClose handle

                events <- S.toList_ $ S.take 1 $ collectFromFile testOptions path
                case events of
                    [event] -> line event `shouldBe` T.pack "test\xFFFD"
                    _ -> expectationFailure "Expected exactly one event"

        it "handles mixed valid and invalid UTF-8" $ do
            withSystemTempFile "utf8_mixed.log" $ \path handle -> do
                -- Write "caf" + invalid 0xFF + valid UTF-8 "é" (0xC3 0xA9) + newline
                BS.hPut handle (BS.pack [0x63, 0x61, 0x66, 0xFF, 0xC3, 0xA9, 0x0A])
                hClose handle

                events <- S.toList_ $ S.take 1 $ collectFromFile testOptions path
                case events of
                    [event] -> line event `shouldBe` T.pack "caf\xFFFD\x00E9"
                    _ -> expectationFailure "Expected exactly one event"

        it "handles incomplete 2-byte UTF-8 sequence at end of line" $ do
            withSystemTempFile "utf8_incomplete_2byte.log" $ \path handle -> do
                -- Write "abc" + start of 2-byte sequence (0xC3) without continuation + newline
                BS.hPut handle (BS.pack [0x61, 0x62, 0x63, 0xC3, 0x0A])
                hClose handle

                events <- S.toList_ $ S.take 1 $ collectFromFile testOptions path
                case events of
                    [event] -> line event `shouldBe` T.pack "abc\xFFFD"
                    _ -> expectationFailure "Expected exactly one event"

        it "handles multiple consecutive invalid bytes" $ do
            withSystemTempFile "utf8_consecutive_invalid.log" $ \path handle -> do
                -- Write "hi" + three invalid bytes (0xFF, 0xFE, 0x80) + "bye" + newline
                -- 0xFF, 0xFE are never valid in UTF-8; 0x80 is a continuation byte without a start byte
                BS.hPut handle (BS.pack [0x68, 0x69, 0xFF, 0xFE, 0x80, 0x62, 0x79, 0x65, 0x0A])
                hClose handle

                events <- S.toList_ $ S.take 1 $ collectFromFile testOptions path
                -- Each invalid byte becomes U+FFFD (replacement character)
                let replacementChar = '\xFFFD'
                let expected = T.pack $ "hi" ++ [replacementChar, replacementChar, replacementChar] ++ "bye"
                case events of
                    [event] -> line event `shouldBe` expected
                    _ -> expectationFailure "Expected exactly one event"

        it "handles valid UTF-8 with multibyte characters correctly" $ do
            withSystemTempFile "utf8_valid_multibyte.log" $ \path handle -> do
                -- Write valid UTF-8: "日本語" (Japanese) + newline
                -- 日 = 0xE6 0x97 0xA5
                -- 本 = 0xE6 0x9C 0xAC
                -- 語 = 0xE8 0xAA 0x9E
                BS.hPut handle (BS.pack [0xE6, 0x97, 0xA5, 0xE6, 0x9C, 0xAC, 0xE8, 0xAA, 0x9E, 0x0A])
                hClose handle

                events <- S.toList_ $ S.take 1 $ collectFromFile testOptions path
                case events of
                    [event] -> line event `shouldBe` T.pack "日本語"
                    _ -> expectationFailure "Expected exactly one event"

        it "handles emoji (4-byte UTF-8 sequences)" $ do
            withSystemTempFile "utf8_emoji.log" $ \path handle -> do
                -- Write "test " + 🎉 (0xF0 0x9F 0x8E 0x89) + newline
                BS.hPut handle (BS.pack [0x74, 0x65, 0x73, 0x74, 0x20, 0xF0, 0x9F, 0x8E, 0x89, 0x0A])
                hClose handle

                events <- S.toList_ $ S.take 1 $ collectFromFile testOptions path
                case events of
                    [event] -> line event `shouldBe` T.pack "test \x1F389"
                    _ -> expectationFailure "Expected exactly one event"

        it "handles truncated 4-byte UTF-8 sequence" $ do
            withSystemTempFile "utf8_truncated_4byte.log" $ \path handle -> do
                -- Write "ok" + incomplete emoji (only first 2 bytes of 4-byte sequence) + newline
                BS.hPut handle (BS.pack [0x6F, 0x6B, 0xF0, 0x9F, 0x0A])
                hClose handle

                events <- S.toList_ $ S.take 1 $ collectFromFile testOptions path
                -- lenientDecode replaces each invalid byte with replacement char
                case events of
                    [event] -> line event `shouldBe` T.pack "ok\xFFFD\xFFFD"
                    _ -> expectationFailure "Expected exactly one event"

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
