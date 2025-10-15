module Main (main) where

import Control.Concurrent (forkIO, threadDelay)
import Control.Exception (SomeException, try)
import qualified Data.Text as T
import System.IO (IOMode (..), hClose, hPutStrLn, openFile, stderr)
import System.IO.Temp (emptySystemTempFile)
import System.Process (callCommand)
import Test.Hspec (Spec, describe, expectationFailure, it, shouldBe)
import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.Hspec (testSpec)

import qualified Streaming.Prelude as S

import Collect (collectFromFile)
import Types (CollectOptions (..), CollectEvent (..))

main :: IO ()
main = do
  specs <- mapM (uncurry testSpec)
    [ ("Unit Tests", unitTests)
    ]
  defaultMain (testGroup "SpanShot Tests" specs)

testOptions :: CollectOptions
testOptions = CollectOptions { pollIntervalMs = 50 }

unitTests :: Spec
unitTests = do
  describe "collectFromFile" $ do
    it "reads all lines from a static file" $ do
      events <- S.toList_ $ S.take 5 $ collectFromFile testOptions "test/fixtures/small.log"
      length events `shouldBe` 5
      map sessionOrderId events `shouldBe` [0, 1, 2, 3, 4]
      map line events `shouldBe`
        [ T.pack "first line"
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

    it "streams appended lines (polling behavior)" $ do
      tempFile <- emptySystemTempFile "stream_test.log"
      logInTest $ "Temp file: " <> tempFile
      
      -- Write initial lines
      handle <- openFile tempFile WriteMode
      hPutStrLn handle "line 1"
      hPutStrLn handle "line 2"
      hClose handle
      
      -- Spawn separate process to append lines after delay
      _ <- forkIO $ do
        threadDelay 100000  -- 100ms delay
        -- Use shell command in separate process to append
        callCommand $ "echo 'line 3' >> " ++ tempFile
        callCommand $ "echo 'line 4' >> " ++ tempFile
        callCommand $ "echo 'line 5' >> " ++ tempFile
      
      events <- S.toList_ $ S.take 5 $ collectFromFile testOptions tempFile
      length events `shouldBe` 5
      map sessionOrderId events `shouldBe` [0, 1, 2, 3, 4]
      map line events `shouldBe` [T.pack "line 1", T.pack "line 2", T.pack "line 3", T.pack "line 4", T.pack "line 5"]

logInTest :: String -> IO ()
logInTest msg = hPutStrLn stderr $ "\n" <> msg
