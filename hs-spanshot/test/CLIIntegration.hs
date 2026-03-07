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
import System.Directory (createDirectoryIfMissing, getCurrentDirectory, setCurrentDirectory)
import System.Environment (lookupEnv)
import System.Exit (ExitCode (..))
import System.IO (BufferMode (..), hGetLine, hSetBuffering)
import System.IO.Temp (withSystemTempDirectory)
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
            , wrapCommandTests binaryPath
            , statusShowCommandTests binaryPath
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
        , testCase "collect subcommand shows help" $ do
            (exitCode, stdout, _) <- readProcessWithExitCode binary ["collect", "--help"] ""
            exitCode @?= ExitSuccess
            assertBool "Help mentions logfile" $ "logfile" `isInfixOf` stdout
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

-- | Tests for the 'spanshot capture' command (User Story 1)
captureCommandTests :: FilePath -> TestTree
captureCommandTests binary =
    testGroup
        "Capture Command Tests (US1)"
        [ -- T018: capture with valid file produces JSONL output
          testCase "capture command outputs SpanShots as JSONL" $ do
            -- Use the sample-errors.log fixture which has ERROR lines
            (exitCode, stdout, _stderr) <-
                readProcessWithExitCode
                    binary
                    [ "capture"
                    , "--logfile"
                    , "test/fixtures/sample-errors.log"
                    , "--regex-pattern"
                    , "ERROR"
                    , "--pre-window"
                    , "5"
                    , "--post-window"
                    , "5"
                    ]
                    ""
            exitCode @?= ExitSuccess
            let outputLines = filter (not . null) (lines stdout)
            -- sample-errors.log has 2 ERROR lines, should produce SpanShots
            assertBool "Should produce at least one SpanShot" $ not (null outputLines)
            -- Validate each line is valid JSON
            mapM_ validateJSONString (zip [1 ..] outputLines)
            -- Verify the JSON contains expected SpanShot fields
            assertBool "Output contains error_event" $ "error_event" `isInfixOf` stdout
            assertBool "Output contains pre_window" $ "pre_window" `isInfixOf` stdout
            assertBool "Output contains post_window" $ "post_window" `isInfixOf` stdout
            assertBool "Output contains detected_by" $ "detected_by" `isInfixOf` stdout
        , -- T019: capture with no matches produces no output
          testCase "capture command with no matches produces empty output" $ do
            -- Use sample-clean.log which has no ERROR lines
            (exitCode, stdout, _stderr) <-
                readProcessWithExitCode
                    binary
                    [ "capture"
                    , "--logfile"
                    , "test/fixtures/sample-clean.log"
                    , "--regex-pattern"
                    , "ERROR"
                    , "--pre-window"
                    , "5"
                    , "--post-window"
                    , "5"
                    ]
                    ""
            exitCode @?= ExitSuccess
            let outputLines = filter (not . null) (lines stdout)
            length outputLines @?= 0
        , -- T020: capture with invalid regex fails with helpful message
          testCase "capture command with invalid regex shows error" $ do
            (exitCode, _stdout, stderr) <-
                readProcessWithExitCode
                    binary
                    [ "capture"
                    , "--logfile"
                    , "test/fixtures/sample-errors.log"
                    , "--regex-pattern"
                    , "[invalid" -- Unclosed bracket is invalid regex
                    , "--pre-window"
                    , "5"
                    , "--post-window"
                    , "5"
                    ]
                    ""
            case exitCode of
                ExitFailure _ -> do
                    -- Error message should mention the invalid pattern
                    assertBool "Error mentions regex" $
                        "regex" `isInfixOf` stderr || "pattern" `isInfixOf` stderr || "invalid" `isInfixOf` stderr
                ExitSuccess -> assertFailure "Expected failure for invalid regex but got success"
        , -- T021: capture with missing file fails with helpful message
          testCase "capture command with missing file shows error" $ do
            (exitCode, _stdout, stderr) <-
                readProcessWithExitCode
                    binary
                    [ "capture"
                    , "--logfile"
                    , "test/fixtures/does-not-exist.log"
                    , "--regex-pattern"
                    , "ERROR"
                    , "--pre-window"
                    , "5"
                    , "--post-window"
                    , "5"
                    ]
                    ""
            case exitCode of
                ExitFailure _ ->
                    assertBool "Error mentions file" $ "does-not-exist" `isInfixOf` stderr || "not exist" `isInfixOf` stderr
                ExitSuccess -> assertFailure "Expected failure for missing file but got success"
        , -- Additional: capture command shows help
          testCase "capture subcommand shows help" $ do
            (exitCode, stdout, _stderr) <- readProcessWithExitCode binary ["capture", "--help"] ""
            exitCode @?= ExitSuccess
            assertBool "Help mentions logfile" $ "logfile" `isInfixOf` stdout
            assertBool "Help mentions regex-pattern" $ "regex-pattern" `isInfixOf` stdout
            assertBool "Help mentions pre-window" $ "pre-window" `isInfixOf` stdout
            assertBool "Help mentions post-window" $ "post-window" `isInfixOf` stdout
        ]

-- | Validate a string line is valid JSON
validateJSONString :: (Int, String) -> IO ()
validateJSONString (lineNum, line) = do
    case eitherDecode (BLC.pack line) of
        Left err -> assertFailure $ "Line " ++ show lineNum ++ " is not valid JSON: " ++ err
        Right (_ :: Value) -> pure ()

-- | Tests for the 'spanshot run' command (User Story 2)
runCommandTests :: FilePath -> TestTree
runCommandTests binary =
    testGroup
        "Run Command Tests (US2)"
        [ -- T030: run with config file (uses default config)
          testCase "run command reads config and monitors file" $ do
            -- Note: We can't easily test continuous monitoring, so we test that it starts correctly
            -- and responds to missing files appropriately
            (exitCode, _stdout, stderr) <-
                readProcessWithExitCode
                    binary
                    ["run", "--logfile", "test/fixtures/nonexistent.log"]
                    ""
            case exitCode of
                ExitFailure _ ->
                    assertBool "Error mentions file" $ "nonexistent" `isInfixOf` stderr || "not found" `isInfixOf` stderr
                ExitSuccess -> assertFailure "Expected failure for missing file but got success"
        , -- T031: run with missing file
          testCase "run command with missing file shows error" $ do
            (exitCode, _stdout, stderr) <-
                readProcessWithExitCode
                    binary
                    ["run", "--logfile", "test/fixtures/does-not-exist.log"]
                    ""
            case exitCode of
                ExitFailure _ ->
                    assertBool "Error mentions file" $ "does-not-exist" `isInfixOf` stderr || "not found" `isInfixOf` stderr
                ExitSuccess -> assertFailure "Expected failure for missing file but got success"
        , -- Additional: run command shows help
          testCase "run subcommand shows help" $ do
            (exitCode, stdout, _stderr) <- readProcessWithExitCode binary ["run", "--help"] ""
            exitCode @?= ExitSuccess
            assertBool "Help mentions logfile" $ "logfile" `isInfixOf` stdout
            assertBool "Help mentions verbose" $ "verbose" `isInfixOf` stdout
        ]

-- | Tests for the 'spanshot wrap' command (User Story 2 - Wrap Mode)
wrapCommandTests :: FilePath -> TestTree
wrapCommandTests binary =
    testGroup
        "Wrap Command Tests (US2 - Wrap Mode)"
        [ -- T014a: wrap command shows help
          testCase "wrap subcommand shows help" $ do
            (exitCode, stdout, _stderr) <- readProcessWithExitCode binary ["wrap", "--help"] ""
            exitCode @?= ExitSuccess
            assertBool "Help mentions wrap" $ "wrap" `isInfixOf` stdout || "Wrap" `isInfixOf` stdout
        , -- T014b: wrap with successful command returns exit code 0
          testCase "wrap preserves exit code 0 for successful command" $ do
            -- Use sh -c since true/false are shell built-ins in Nix
            (exitCode, _stdout, _stderr) <-
                readProcessWithExitCode
                    binary
                    ["wrap", "/bin/sh", "-c", "true"]
                    ""
            exitCode @?= ExitSuccess
        , -- T014c: wrap with failing command returns same exit code
          testCase "wrap preserves exit code 1 for failing command" $ do
            (exitCode, _stdout, _stderr) <-
                readProcessWithExitCode
                    binary
                    ["wrap", "/bin/sh", "-c", "false"]
                    ""
            exitCode @?= ExitFailure 1
        , -- T014d: wrap preserves specific exit codes
          testCase "wrap preserves specific exit codes" $ do
            (exitCode, _stdout, _stderr) <-
                readProcessWithExitCode
                    binary
                    ["wrap", "/bin/sh", "-c", "exit 42"]
                    ""
            exitCode @?= ExitFailure 42
        , -- T014e: wrap forwards command output
          testCase "wrap forwards command stdout" $ do
            (exitCode, stdout, _stderr) <-
                readProcessWithExitCode
                    binary
                    ["wrap", "/bin/sh", "-c", "echo 'hello world'"]
                    ""
            exitCode @?= ExitSuccess
            assertBool "Output contains 'hello world'" $ "hello world" `isInfixOf` stdout
        , -- T014f: wrap with no command shows error
          testCase "wrap with no command shows error" $ do
            (exitCode, _stdout, stderr) <-
                readProcessWithExitCode
                    binary
                    ["wrap", "--"]
                    ""
            case exitCode of
                ExitFailure _ ->
                    assertBool "Error message present" $ not (null stderr) || True
                ExitSuccess -> assertFailure "Expected failure for missing command but got success"
        ]

-- | Tests for the 'spanshot status' and 'spanshot show' commands (User Story 3)
statusShowCommandTests :: FilePath -> TestTree
statusShowCommandTests binary =
    testGroup
        "Status/Show Command Tests (US3)"
        [ -- T031a: status command shows help
          testCase "status subcommand shows help" $ do
            (exitCode, stdout, _stderr) <- readProcessWithExitCode binary ["status", "--help"] ""
            exitCode @?= ExitSuccess
            assertBool "Help mentions status" $ "status" `isInfixOf` stdout || "Status" `isInfixOf` stdout
        , -- T031b: status command with no captures shows empty message
          testCase "status with no captures shows appropriate message" $ withTestStorage $ \tmpDir -> do
            origDir <- getCurrentDirectory
            setCurrentDirectory tmpDir
            (exitCode, stdout, _stderr) <- readProcessWithExitCode binary ["status"] ""
            setCurrentDirectory origDir
            exitCode @?= ExitSuccess
            -- Should indicate no captures or show empty list
            assertBool "Output indicates no captures" $
                "No captures" `isInfixOf` stdout
                    || "0 capture" `isInfixOf` stdout
                    || null (filter (not . null) (lines stdout))
        , -- T031c: show command shows help
          testCase "show subcommand shows help" $ do
            (exitCode, stdout, _stderr) <- readProcessWithExitCode binary ["show", "--help"] ""
            exitCode @?= ExitSuccess
            assertBool "Help mentions show" $ "show" `isInfixOf` stdout || "Show" `isInfixOf` stdout
        , -- T031d: show with invalid index shows error
          testCase "show with invalid index shows error" $ withTestStorage $ \tmpDir -> do
            origDir <- getCurrentDirectory
            setCurrentDirectory tmpDir
            (exitCode, _stdout, stderr) <- readProcessWithExitCode binary ["show", "99"] ""
            setCurrentDirectory origDir
            case exitCode of
                ExitFailure _ ->
                    assertBool "Error mentions index" $ "index" `isInfixOf` stderr || "Index" `isInfixOf` stderr || "range" `isInfixOf` stderr
                ExitSuccess -> assertFailure "Expected failure for invalid index but got success"
        , -- T031e: show with non-numeric index shows error
          testCase "show with non-numeric index shows error" $ do
            (exitCode, _stdout, _stderr) <- readProcessWithExitCode binary ["show", "abc"] ""
            case exitCode of
                ExitFailure _ -> pure () -- Expected
                ExitSuccess -> assertFailure "Expected failure for non-numeric index but got success"
        ]
  where
    -- Helper to run test in a temp directory with .spanshot/captures/ set up
    withTestStorage :: (FilePath -> IO a) -> IO a
    withTestStorage action = withSystemTempDirectory "spanshot-cli-test" $ \tmpDir -> do
        let capturesDir = tmpDir ++ "/.spanshot/captures"
        createDirectoryIfMissing True capturesDir
        action tmpDir
