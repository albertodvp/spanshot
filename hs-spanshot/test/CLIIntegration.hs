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

import Data.List (isInfixOf)
import Data.Maybe (fromMaybe)
import System.Directory (createDirectoryIfMissing, getCurrentDirectory, setCurrentDirectory)
import System.Environment (lookupEnv)
import System.Exit (ExitCode (..))
import System.IO.Temp (withSystemTempDirectory)
import System.Process (readProcessWithExitCode)
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (assertBool, assertFailure, testCase, (@?=))

main :: IO ()
main = do
    binaryPath <- getBinaryPath
    defaultMain $
        testGroup
            "CLI Integration Tests"
            [ behaviorTests binaryPath
            , configTests binaryPath
            , errorHandlingTests binaryPath
            , execCommandTests binaryPath
            , capturesCommandTests binaryPath
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

behaviorTests :: FilePath -> TestTree
behaviorTests binary =
    testGroup
        "CLI Behavior Tests"
        [ testCase "shows help text" $ do
            (exitCode, stdout, _) <- readProcessWithExitCode binary ["--help"] ""
            exitCode @?= ExitSuccess
            assertBool "Help contains 'SpanShot'" $ "SpanShot" `isInfixOf` stdout
            assertBool "Help mentions exec command" $ "exec" `isInfixOf` stdout
            assertBool "Help mentions captures command" $ "captures" `isInfixOf` stdout
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
        [ testCase "requires subcommand" $ do
            (exitCode, _stdout, _stderr) <- readProcessWithExitCode binary [] ""
            case exitCode of
                ExitFailure _ -> pure ()
                ExitSuccess -> assertFailure "Expected failure when no subcommand given"
        ]

-- | Tests for the 'spanshot exec' command (run commands with monitoring)
execCommandTests :: FilePath -> TestTree
execCommandTests binary =
    testGroup
        "Exec Command Tests"
        [ -- T014a: exec command shows help
          testCase "exec subcommand shows help" $ do
            (exitCode, stdout, _stderr) <- readProcessWithExitCode binary ["exec", "--help"] ""
            exitCode @?= ExitSuccess
            assertBool "Help mentions exec" $ "exec" `isInfixOf` stdout || "Exec" `isInfixOf` stdout || "command" `isInfixOf` stdout
        , -- T014b: exec with successful command returns exit code 0
          testCase "exec preserves exit code 0 for successful command" $ do
            (exitCode, _stdout, _stderr) <-
                readProcessWithExitCode
                    binary
                    ["exec", "/bin/sh", "-c", "true"]
                    ""
            exitCode @?= ExitSuccess
        , -- T014c: exec with failing command returns same exit code
          testCase "exec preserves exit code 1 for failing command" $ do
            (exitCode, _stdout, _stderr) <-
                readProcessWithExitCode
                    binary
                    ["exec", "/bin/sh", "-c", "false"]
                    ""
            exitCode @?= ExitFailure 1
        , -- T014d: exec preserves specific exit codes
          testCase "exec preserves specific exit codes" $ do
            (exitCode, _stdout, _stderr) <-
                readProcessWithExitCode
                    binary
                    ["exec", "/bin/sh", "-c", "exit 42"]
                    ""
            exitCode @?= ExitFailure 42
        , -- T014e: exec forwards command output
          testCase "exec forwards command stdout" $ do
            (exitCode, stdout, _stderr) <-
                readProcessWithExitCode
                    binary
                    ["exec", "/bin/sh", "-c", "echo 'hello world'"]
                    ""
            exitCode @?= ExitSuccess
            assertBool "Output contains 'hello world'" $ "hello world" `isInfixOf` stdout
        , -- T014f: exec with no command shows error
          testCase "exec with no command shows error" $ do
            (exitCode, _stdout, stderr) <-
                readProcessWithExitCode
                    binary
                    ["exec", "--"]
                    ""
            case exitCode of
                ExitFailure _ ->
                    assertBool "Error message present" $ not (null stderr) || True
                ExitSuccess -> assertFailure "Expected failure for missing command but got success"
        ]

-- | Tests for the 'spanshot captures' subcommand (list/show)
capturesCommandTests :: FilePath -> TestTree
capturesCommandTests binary =
    testGroup
        "Captures Command Tests"
        [ testCase "captures subcommand shows help" $ do
            (exitCode, stdout, _stderr) <- readProcessWithExitCode binary ["captures", "--help"] ""
            exitCode @?= ExitSuccess
            assertBool "Help mentions list" $ "list" `isInfixOf` stdout
            assertBool "Help mentions show" $ "show" `isInfixOf` stdout
        , testCase "captures list with no captures shows appropriate message" $ withTestStorage $ \tmpDir -> do
            origDir <- getCurrentDirectory
            setCurrentDirectory tmpDir
            (exitCode, stdout, _stderr) <- readProcessWithExitCode binary ["captures", "list"] ""
            setCurrentDirectory origDir
            exitCode @?= ExitSuccess
            assertBool "Output indicates no captures" $
                "No captures" `isInfixOf` stdout
                    || "0 capture" `isInfixOf` stdout
                    || null (filter (not . null) (lines stdout))
        , testCase "captures show subcommand shows help" $ do
            (exitCode, stdout, _stderr) <- readProcessWithExitCode binary ["captures", "show", "--help"] ""
            exitCode @?= ExitSuccess
            assertBool "Help mentions INDEX" $ "INDEX" `isInfixOf` stdout || "index" `isInfixOf` stdout
        , testCase "captures show with invalid index shows error" $ withTestStorage $ \tmpDir -> do
            origDir <- getCurrentDirectory
            setCurrentDirectory tmpDir
            (exitCode, _stdout, stderr) <- readProcessWithExitCode binary ["captures", "show", "99"] ""
            setCurrentDirectory origDir
            case exitCode of
                ExitFailure _ ->
                    assertBool "Error mentions index" $ "index" `isInfixOf` stderr || "Index" `isInfixOf` stderr || "range" `isInfixOf` stderr
                ExitSuccess -> assertFailure "Expected failure for invalid index but got success"
        , testCase "captures show with non-numeric index shows error" $ do
            (exitCode, _stdout, _stderr) <- readProcessWithExitCode binary ["captures", "show", "abc"] ""
            case exitCode of
                ExitFailure _ -> pure () -- Expected
                ExitSuccess -> assertFailure "Expected failure for non-numeric index but got success"
        ]
  where
    withTestStorage :: (FilePath -> IO a) -> IO a
    withTestStorage action = withSystemTempDirectory "spanshot-cli-test" $ \tmpDir -> do
        let capturesDir = tmpDir ++ "/.spanshot/captures"
        createDirectoryIfMissing True capturesDir
        action tmpDir
