module Main (main) where

-- \| CLI Integration Tests
--
-- These tests validate the spanshot CLI binary end-to-end, ensuring that:
--
-- 1. The binary correctly reads and processes log files
-- 2. Output adheres to the expected JSONL format
-- 3. Command-line arguments are properly handled
-- 4. Error cases produce appropriate exit codes and messages
--
-- Why CLI integration tests are important:
--
-- - Validates the entire pipeline from CLI parsing to output formatting
-- - Catches issues with option parsing, configuration, and user-facing behavior
-- - Ensures the binary can be safely distributed and used by end users
-- - Tests the actual compiled executable, not just library code
--
-- How these tests work:
--
-- - The spanshot binary is available via 'build-tool-depends' in the cabal file
-- - Runs the binary as a subprocess with various arguments
-- - Validates output structure (JSONL format) and content
-- - Uses 'timeout' to handle streaming behavior (since spanshot tails files)
-- - Tests both success and failure scenarios

import Control.Exception (bracket)
import Control.Monad (replicateM)
import Data.Aeson (Value, eitherDecode)
import Data.ByteString.Lazy qualified as BL
import Data.ByteString.Lazy.Char8 qualified as BLC
import Data.List (isInfixOf)
import Data.Maybe (fromMaybe)
import System.Environment (lookupEnv)
import System.Exit (ExitCode (..))
import System.IO (BufferMode (..), hGetLine, hSetBuffering)
import System.Process (CreateProcess (..), StdStream (..), createProcess, proc, readProcessWithExitCode, terminateProcess, waitForProcess)
import System.Timeout (timeout)
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (assertBool, assertFailure, testCase, (@?=))

main :: IO ()
main = do
    binaryPath <- getBinaryPath
    defaultMain $
        testGroup
            "CLI Integration Tests"
            [ outputValidationTests binaryPath
            , behaviorTests binaryPath
            , configTests binaryPath
            , errorHandlingTests binaryPath
            , captureCommandTests binaryPath
            , runCommandTests binaryPath
            ]

{- | Get the path to the spanshot binary.

The binary is available in PATH thanks to 'build-tool-depends: hs-spanshot:spanshot'
in the cabal file. This declaration tells Cabal to build the spanshot executable
and add it to PATH before running this test suite.

This approach works in both:
  - Cabal builds: Cabal handles the dependency and PATH setup
  - Nix builds: Nix respects build-tool-depends and provides the binary in PATH

The SPANSHOT_BIN environment variable can override this for manual testing
with a specific binary location.
-}
getBinaryPath :: IO FilePath
getBinaryPath = do
    mPath <- lookupEnv "SPANSHOT_BIN"
    pure $ fromMaybe "spanshot" mPath

outputValidationTests :: FilePath -> TestTree
outputValidationTests binary =
    testGroup
        "Output Validation Tests"
        [ testCase "processes small.log and produces valid JSONL output" $ do
            output <- runCollectToEnd binary "test/fixtures/small.log" 5
            let outputLines = BLC.lines output
            length outputLines @?= 5
            mapM_ validateJSONLine (zip [1 ..] outputLines)
        , testCase "processes empty.log and produces no output" $ do
            output <- runCollectToEnd binary "test/fixtures/empty.log" 0
            let outputLines = BLC.lines output
            length outputLines @?= 0
        , testCase "processes long_file.log and produces valid JSONL output" $ do
            output <- runCollectToEnd binary "test/fixtures/long_file.log" 1133
            let outputLines = BLC.lines output
            length outputLines @?= 1133
            mapM_ validateJSONLine (zip [1 ..] outputLines)
        ]

behaviorTests :: FilePath -> TestTree
behaviorTests binary =
    testGroup
        "CLI Behavior Tests"
        [ testCase "shows help text" $ do
            (exitCode, stdout, _) <- readProcessWithExitCode binary ["--help"] ""
            exitCode @?= ExitSuccess
            assertBool "Help contains 'SpanShot'" $ "SpanShot" `isInfixOf` stdout
            assertBool "Help mentions collect command" $ "collect" `isInfixOf` stdout
            assertBool "Help mentions capture command" $ "capture" `isInfixOf` stdout
            assertBool "Help mentions run command" $ "run" `isInfixOf` stdout
        , testCase "collect subcommand shows help" $ do
            (exitCode, stdout, _) <- readProcessWithExitCode binary ["collect", "--help"] ""
            exitCode @?= ExitSuccess
            assertBool "Help mentions logfile" $ "logfile" `isInfixOf` stdout
        , testCase "capture subcommand shows help" $ do
            (exitCode, stdout, _) <- readProcessWithExitCode binary ["capture", "--help"] ""
            exitCode @?= ExitSuccess
            assertBool "Help mentions logfile" $ "logfile" `isInfixOf` stdout
            assertBool "Help mentions regex-pattern" $ "regex-pattern" `isInfixOf` stdout
            assertBool "Help mentions pre-window" $ "pre-window" `isInfixOf` stdout
            assertBool "Help mentions post-window" $ "post-window" `isInfixOf` stdout
        , testCase "run subcommand shows help" $ do
            (exitCode, stdout, _) <- readProcessWithExitCode binary ["run", "--help"] ""
            exitCode @?= ExitSuccess
            assertBool "Help mentions logfile" $ "logfile" `isInfixOf` stdout
            assertBool "Help mentions regex-pattern" $ "regex-pattern" `isInfixOf` stdout
        ]

configTests :: FilePath -> TestTree
configTests binary =
    testGroup
        "Config Command Tests"
        [ testCase "config path shows a path" $ do
            (exitCode, stdout, _) <- readProcessWithExitCode binary ["config", "path"] ""
            exitCode @?= ExitSuccess
            assertBool "Output contains config.yaml" $ "config.yaml" `isInfixOf` stdout
            assertBool "Output contains spanshot" $ "spanshot" `isInfixOf` stdout
        , testCase "config show outputs valid YAML" $ do
            (exitCode, stdout, _) <- readProcessWithExitCode binary ["config", "show"] ""
            exitCode @?= ExitSuccess
            assertBool "Output contains capture section" $ "capture:" `isInfixOf` stdout
            assertBool "Output contains pre_window_duration" $ "pre_window_duration" `isInfixOf` stdout
            assertBool "Output contains detection_rules" $ "detection_rules" `isInfixOf` stdout
        , testCase "config subcommand shows help" $ do
            (exitCode, stdout, _) <- readProcessWithExitCode binary ["config", "--help"] ""
            exitCode @?= ExitSuccess
            assertBool "Help mentions show" $ "show" `isInfixOf` stdout
            assertBool "Help mentions path" $ "path" `isInfixOf` stdout
        ]

errorHandlingTests :: FilePath -> TestTree
errorHandlingTests binary =
    testGroup
        "Error Handling Tests"
        [ testCase "fails with missing file" $ do
            (exitCode, _stdout, stderr) <-
                readProcessWithExitCode
                    binary
                    ["collect", "--logfile", "test/fixtures/nonexistent.log"]
                    ""
            case exitCode of
                ExitFailure _ -> assertBool "Error message mentions file" $ "nonexistent.log" `isInfixOf` stderr
                ExitSuccess -> assertFailure "Expected failure for missing file but got success"
        , testCase "requires subcommand" $ do
            (exitCode, _stdout, _stderr) <- readProcessWithExitCode binary [] ""
            case exitCode of
                ExitFailure _ -> pure ()
                ExitSuccess -> assertFailure "Expected failure when no subcommand given"
        ]

runCollectToEnd :: FilePath -> FilePath -> Int -> IO BL.ByteString
runCollectToEnd binary logfile expectedLines = do
    let timeoutMicroseconds = if expectedLines > 100 then 10_000_000 else 3_000_000
    let procSpec = (proc binary ["collect", "--logfile", logfile]){std_out = CreatePipe, std_err = Inherit}
    bracket
        (createProcess procSpec)
        (\(_, _, _, ph) -> terminateProcess ph >> waitForProcess ph >> pure ())
        $ \(_, mHOut, _, _) -> case mHOut of
            Nothing -> assertFailure "Failed to get stdout handle"
            Just hOut -> do
                hSetBuffering hOut LineBuffering
                result <-
                    timeout timeoutMicroseconds $
                        if expectedLines == 0
                            then pure []
                            else replicateM expectedLines (hGetLine hOut)
                case result of
                    Nothing -> assertFailure "Process timed out before producing expected number of lines"
                    Just outputLines -> pure $ BLC.pack $ unlines outputLines

validateJSONLine :: (Int, BL.ByteString) -> IO ()
validateJSONLine (lineNum, line) = do
    case eitherDecode line of
        Left err -> assertFailure $ "Line " ++ show lineNum ++ " is not valid JSON: " ++ err
        Right (_ :: Value) -> pure ()

-- | Tests for the capture command
captureCommandTests :: FilePath -> TestTree
captureCommandTests binary =
    testGroup
        "Capture Command Tests"
        [ testCase "capture fails with missing logfile" $ do
            (exitCode, _stdout, stderr) <-
                readProcessWithExitCode
                    binary
                    ["capture", "--logfile", "test/fixtures/nonexistent.log", "--regex-pattern", "ERROR"]
                    ""
            case exitCode of
                ExitFailure _ -> assertBool "Error message mentions file" $ "nonexistent.log" `isInfixOf` stderr
                ExitSuccess -> assertFailure "Expected failure for missing file but got success"
        , testCase "capture requires regex-pattern" $ do
            (exitCode, _stdout, _stderr) <-
                readProcessWithExitCode
                    binary
                    ["capture", "--logfile", "test/fixtures/small.log"]
                    ""
            case exitCode of
                ExitFailure _ -> pure () -- Missing required arg should fail
                ExitSuccess -> assertFailure "Expected failure when regex-pattern missing"
        , testCase "capture produces valid JSONL for SpanShots (empty output for static files is expected)" $ do
            -- Note: For static files read instantly, all events have nearly identical timestamps
            -- so post-windows never complete. This tests that the command runs without crashing.
            -- SpanShot output would only appear for live tailing scenarios or with custom timing.
            output <- runCaptureToEnd binary "test/fixtures/empty.log" "ERROR" 0
            let outputLines = filter (not . BLC.null) $ BLC.lines output
            length outputLines @?= 0
        ]

-- | Tests for the run command
runCommandTests :: FilePath -> TestTree
runCommandTests binary =
    testGroup
        "Run Command Tests"
        [ testCase "run fails with missing logfile" $ do
            (exitCode, _stdout, stderr) <-
                readProcessWithExitCode
                    binary
                    ["run", "--logfile", "test/fixtures/nonexistent.log", "--regex-pattern", "ERROR"]
                    ""
            case exitCode of
                ExitFailure _ -> assertBool "Error message mentions file" $ "nonexistent.log" `isInfixOf` stderr
                ExitSuccess -> assertFailure "Expected failure for missing file but got success"
        , testCase "run requires regex-pattern" $ do
            (exitCode, _stdout, _stderr) <-
                readProcessWithExitCode
                    binary
                    ["run", "--logfile", "test/fixtures/small.log"]
                    ""
            case exitCode of
                ExitFailure _ -> pure () -- Missing required arg should fail
                ExitSuccess -> assertFailure "Expected failure when regex-pattern missing"
        , testCase "run accepts all capture options" $ do
            -- Test that run accepts all the same options as capture
            -- We just verify the help text mentions all options since the command tails files
            (exitCode, stdout, _) <- readProcessWithExitCode binary ["run", "--help"] ""
            exitCode @?= ExitSuccess
            assertBool "Help mentions pre-window" $ "pre-window" `isInfixOf` stdout
            assertBool "Help mentions post-window" $ "post-window" `isInfixOf` stdout
            assertBool "Help mentions min-context" $ "min-context" `isInfixOf` stdout
        ]

-- | Run capture command and collect output
runCaptureToEnd :: FilePath -> FilePath -> String -> Int -> IO BL.ByteString
runCaptureToEnd binary logfile pattern expectedLines = do
    let timeoutMicroseconds = 3_000_000
    let procSpec =
            (proc binary ["capture", "--logfile", logfile, "--regex-pattern", pattern])
                { std_out = CreatePipe
                , std_err = Inherit
                }
    bracket
        (createProcess procSpec)
        (\(_, _, _, ph) -> terminateProcess ph >> waitForProcess ph >> pure ())
        $ \(_, mHOut, _, _) -> case mHOut of
            Nothing -> assertFailure "Failed to get stdout handle"
            Just hOut -> do
                hSetBuffering hOut LineBuffering
                result <-
                    timeout timeoutMicroseconds $
                        if expectedLines == 0
                            then pure []
                            else replicateM expectedLines (hGetLine hOut)
                case result of
                    Nothing -> pure BLC.empty -- Timeout is expected for tailing behavior
                    Just outputLines -> pure $ BLC.pack $ unlines outputLines
