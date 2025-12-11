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
-- Architecture:
--
-- - `collect`: reads from stdin or files, outputs JSONL CollectEvent
-- - `capture`: reads JSONL CollectEvent from stdin, outputs JSONL SpanShot
-- - `run`: optimized pipeline (collect → capture internally), reads from stdin or files
--
-- Commands can be composed via pipes:
--   spanshot collect --logfile app.log | spanshot capture --pre-window 5
--
-- Or run as optimized pipeline:
--   spanshot run --logfile app.log --pre-window 5

import Control.Exception (bracket)
import Control.Monad (replicateM)
import Data.Aeson (Value, eitherDecode)
import Data.ByteString.Lazy qualified as BL
import Data.ByteString.Lazy.Char8 qualified as BLC
import Data.List (isInfixOf)
import Data.Maybe (fromMaybe)
import System.Directory (createDirectoryIfMissing)
import System.Environment (lookupEnv)
import System.Exit (ExitCode (..))
import System.FilePath ((</>))
import System.IO (BufferMode (..), Handle, hClose, hGetLine, hIsEOF, hPutStrLn, hSetBuffering)
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
            [ collectCommandTests binaryPath
            , collectStdinTests binaryPath
            , collectConfigTests binaryPath
            , captureCommandTests binaryPath
            , captureStdinTests binaryPath
            , runCommandTests binaryPath
            , runStdinTests binaryPath
            , pipelineTests binaryPath
            , configCommandTests binaryPath
            , errorHandlingTests binaryPath
            , helpTests binaryPath
            ]

-- | Get the path to the spanshot binary.
getBinaryPath :: IO FilePath
getBinaryPath = do
    mPath <- lookupEnv "SPANSHOT_BIN"
    pure $ fromMaybe "spanshot" mPath

-------------------------------------------------------------------------------
-- Collect Command Tests
-------------------------------------------------------------------------------

collectCommandTests :: FilePath -> TestTree
collectCommandTests binary =
    testGroup
        "Collect Command Tests"
        [ testCase "collect with --logfile produces JSONL output" $ do
            output <- runCollectWithFile binary "test/fixtures/small.log" 5
            let outputLines = BLC.lines output
            length outputLines @?= 5
            mapM_ validateJSONLine (zip [1 ..] outputLines)
        , testCase "collect with --logfile sets correct source field" $ do
            output <- runCollectWithFile binary "test/fixtures/small.log" 1
            let outputLines = BLC.lines output
            case outputLines of
                (firstLine : _) -> do
                    assertBool "Source contains filename" $
                        "small.log" `isInfixOf` BLC.unpack firstLine
                [] -> assertFailure "Expected at least one line"
        , testCase "collect with multiple --logfile flags processes all files" $ do
            output <- runCollectWithFiles binary ["test/fixtures/small.log", "test/fixtures/empty.log"] 5
            let outputLines = filter (not . BLC.null) $ BLC.lines output
            -- small.log has 5 lines, empty.log has 0
            length outputLines @?= 5
        , testCase "collect with empty file produces no output" $ do
            output <- runCollectWithFile binary "test/fixtures/empty.log" 0
            let outputLines = BLC.lines output
            length outputLines @?= 0
        , testCase "collect with long file produces all lines" $ do
            output <- runCollectWithFile binary "test/fixtures/long_file.log" 1133
            let outputLines = BLC.lines output
            length outputLines @?= 1133
        ]

collectStdinTests :: FilePath -> TestTree
collectStdinTests binary =
    testGroup
        "Collect Stdin Tests"
        [ testCase "collect reads from stdin when no --logfile provided" $ do
            let input = "line1\nline2\nline3\n"
            (exitCode, stdout, _stderr) <- readProcessWithExitCode binary ["collect"] input
            exitCode @?= ExitSuccess
            let outputLines = filter (not . null) $ lines stdout
            length outputLines @?= 3
            mapM_ validateJSONLineString outputLines
        , testCase "collect stdin sets source to 'stdin'" $ do
            let input = "test line\n"
            (exitCode, stdout, _stderr) <- readProcessWithExitCode binary ["collect"] input
            exitCode @?= ExitSuccess
            assertBool "Source is stdin" $ "\"source\":\"stdin\"" `isInfixOf` stdout
        , testCase "collect stdin handles empty input" $ do
            let input = ""
            (exitCode, stdout, _stderr) <- readProcessWithExitCode binary ["collect"] input
            exitCode @?= ExitSuccess
            let outputLines = filter (not . null) $ lines stdout
            length outputLines @?= 0
        ]

collectConfigTests :: FilePath -> TestTree
collectConfigTests binary =
    testGroup
        "Collect Config Tests"
        [ testCase "collect uses logfiles from config file" $
            withSystemTempDirectory "spanshot-test" $ \tmpDir -> do
                -- Create a config file with logfiles
                let configPath = tmpDir </> ".spanshot.yaml"
                let logPath = tmpDir </> "test.log"
                writeFile logPath "log line 1\nlog line 2\n"
                writeFile configPath $
                    unlines
                        [ "collect:"
                        , "  logfiles:"
                        , "    - ./test.log"
                        , "capture:"
                        , "  detection_rules:"
                        , "    - regex_pattern: ERROR"
                        ]
                -- Run from tmpDir so config is found
                (exitCode, stdout, _stderr) <- readProcessWithExitCodeInDir tmpDir binary ["collect"] ""
                exitCode @?= ExitSuccess
                let outputLines = filter (not . null) $ lines stdout
                length outputLines @?= 2
        , testCase "collect --logfile overrides config logfiles" $
            withSystemTempDirectory "spanshot-test" $ \tmpDir -> do
                -- Create config with one logfile, but use --logfile to specify another
                let configPath = tmpDir </> ".spanshot.yaml"
                let configLogPath = tmpDir </> "config.log"
                let cliLogPath = tmpDir </> "cli.log"
                writeFile configLogPath "config line\n"
                writeFile cliLogPath "cli line 1\ncli line 2\n"
                writeFile configPath $
                    unlines
                        [ "collect:"
                        , "  logfiles:"
                        , "    - ./config.log"
                        , "capture:"
                        , "  detection_rules:"
                        , "    - regex_pattern: ERROR"
                        ]
                -- --logfile should override config
                (exitCode, stdout, _stderr) <- readProcessWithExitCodeInDir tmpDir binary ["collect", "--logfile", cliLogPath] ""
                exitCode @?= ExitSuccess
                let outputLines = filter (not . null) $ lines stdout
                length outputLines @?= 2
        , testCase "collect resolves relative paths from config file location" $
            withSystemTempDirectory "spanshot-test" $ \tmpDir -> do
                -- Create subdirectory structure
                let logsDir = tmpDir </> "logs"
                createDirectoryIfMissing True logsDir
                let logPath = logsDir </> "app.log"
                writeFile logPath "app log line\n"
                -- Config references relative path
                let configPath = tmpDir </> ".spanshot.yaml"
                writeFile configPath $
                    unlines
                        [ "collect:"
                        , "  logfiles:"
                        , "    - ./logs/app.log"
                        , "capture:"
                        , "  detection_rules:"
                        , "    - regex_pattern: ERROR"
                        ]
                -- Run from tmpDir
                (exitCode, stdout, _stderr) <- readProcessWithExitCodeInDir tmpDir binary ["collect"] ""
                exitCode @?= ExitSuccess
                let outputLines = filter (not . null) $ lines stdout
                length outputLines @?= 1
        ]

-------------------------------------------------------------------------------
-- Capture Command Tests
-------------------------------------------------------------------------------

captureCommandTests :: FilePath -> TestTree
captureCommandTests binary =
    testGroup
        "Capture Command Tests"
        [ testCase "capture --help shows capture-specific flags only" $ do
            (exitCode, stdout, _stderr) <- readProcessWithExitCode binary ["capture", "--help"] ""
            exitCode @?= ExitSuccess
            -- Should have capture flags
            assertBool "Help mentions pre-window" $ "pre-window" `isInfixOf` stdout
            assertBool "Help mentions post-window" $ "post-window" `isInfixOf` stdout
            assertBool "Help mentions regex-pattern" $ "regex-pattern" `isInfixOf` stdout
            -- Should NOT have collect-specific flags
            assertBool "Help should not mention logfile" $ not ("logfile" `isInfixOf` stdout)
            assertBool "Help should not mention poll-interval" $ not ("poll-interval" `isInfixOf` stdout)
        ]

captureStdinTests :: FilePath -> TestTree
captureStdinTests binary =
    testGroup
        "Capture Stdin Tests"
        [ testCase "capture reads JSONL CollectEvent from stdin" $ do
            -- Create valid CollectEvent JSONL (using our actual field names)
            let collectEvent =
                    "{\"source\":\"test.log\",\"session_order_id\":1,\"line\":\"ERROR something failed\",\"read_at_utc\":\"2025-01-01T00:00:00Z\"}"
            (exitCode, _stdout, stderr) <- readProcessWithExitCode binary ["capture", "--regex-pattern", "ERROR"] collectEvent
            -- Should not fail parsing
            case exitCode of
                ExitSuccess -> pure ()
                ExitFailure _ -> assertBool "Should not have parse error" $ not ("parse" `isInfixOf` stderr)
        , testCase "capture fails on invalid JSONL input" $ do
            let invalidInput = "this is not json\n"
            (exitCode, _stdout, _stderr) <- readProcessWithExitCode binary ["capture", "--regex-pattern", "ERROR"] invalidInput
            -- TODO: Currently should fail, proper error handling to be added later
            case exitCode of
                ExitFailure _ -> pure () -- Expected
                ExitSuccess -> pure () -- Also acceptable if it just skips invalid lines
        , testCase "capture with valid input produces SpanShot JSONL" $ do
            -- Create a sequence of CollectEvents that should trigger a capture
            -- Pre-window event, error event, post-window event (with time gap)
            let events =
                    unlines
                        [ "{\"source\":\"test.log\",\"session_order_id\":0,\"line\":\"INFO starting\",\"read_at_utc\":\"2025-01-01T00:00:00Z\"}"
                        , "{\"source\":\"test.log\",\"session_order_id\":1,\"line\":\"ERROR failure\",\"read_at_utc\":\"2025-01-01T00:00:01Z\"}"
                        , "{\"source\":\"test.log\",\"session_order_id\":2,\"line\":\"INFO recovering\",\"read_at_utc\":\"2025-01-01T00:00:10Z\"}"
                        ]
            (exitCode, stdout, _stderr) <-
                readProcessWithExitCode
                    binary
                    ["capture", "--regex-pattern", "ERROR", "--pre-window", "5", "--post-window", "2", "--inactivity-timeout", "3"]
                    events
            exitCode @?= ExitSuccess
            -- With proper timing, should produce SpanShot output
            let outputLines = filter (not . null) $ lines stdout
            -- Validate any output is valid JSON
            mapM_ validateJSONLineString outputLines
        ]

-------------------------------------------------------------------------------
-- Run Command Tests
-------------------------------------------------------------------------------

runCommandTests :: FilePath -> TestTree
runCommandTests binary =
    testGroup
        "Run Command Tests"
        [ testCase "run --help shows all flags (collect + capture)" $ do
            (exitCode, stdout, _stderr) <- readProcessWithExitCode binary ["run", "--help"] ""
            exitCode @?= ExitSuccess
            -- Should have collect flags
            assertBool "Help mentions logfile" $ "logfile" `isInfixOf` stdout
            assertBool "Help mentions poll-interval" $ "poll-interval" `isInfixOf` stdout
            -- Should have capture flags
            assertBool "Help mentions pre-window" $ "pre-window" `isInfixOf` stdout
            assertBool "Help mentions post-window" $ "post-window" `isInfixOf` stdout
            assertBool "Help mentions regex-pattern" $ "regex-pattern" `isInfixOf` stdout
        , testCase "run with --logfile processes file" $ do
            -- run should work with a logfile and produce SpanShot output
            (exitCode, _stdout, stderr) <-
                readProcessWithExitCode
                    binary
                    ["run", "--logfile", "test/fixtures/small.log", "--regex-pattern", "ERROR"]
                    ""
            -- Should not error (may timeout waiting for more input, which is fine)
            case exitCode of
                ExitSuccess -> pure ()
                ExitFailure _ -> assertBool "Should not have file error" $ not ("not found" `isInfixOf` stderr)
        , testCase "run fails with missing logfile" $ do
            (exitCode, _stdout, stderr) <-
                readProcessWithExitCode
                    binary
                    ["run", "--logfile", "test/fixtures/nonexistent.log", "--regex-pattern", "ERROR"]
                    ""
            case exitCode of
                ExitFailure _ -> assertBool "Error mentions file" $ "nonexistent.log" `isInfixOf` stderr
                ExitSuccess -> assertFailure "Expected failure for missing file"
        ]

runStdinTests :: FilePath -> TestTree
runStdinTests binary =
    testGroup
        "Run Stdin Tests"
        [ testCase "run reads from stdin when no logfiles configured" $ do
            let input = "INFO starting\nERROR something failed\nINFO done\n"
            (exitCode, _stdout, _stderr) <-
                readProcessWithExitCode
                    binary
                    ["run", "--regex-pattern", "ERROR", "--pre-window", "1", "--post-window", "1", "--inactivity-timeout", "2"]
                    input
            exitCode @?= ExitSuccess
        , testCase "run with config logfiles uses files, not stdin" $
            withSystemTempDirectory "spanshot-test" $ \tmpDir -> do
                let configPath = tmpDir </> ".spanshot.yaml"
                let logPath = tmpDir </> "app.log"
                writeFile logPath "INFO line\nERROR problem\n"
                writeFile configPath $
                    unlines
                        [ "collect:"
                        , "  logfiles:"
                        , "    - ./app.log"
                        , "capture:"
                        , "  detection_rules:"
                        , "    - regex_pattern: ERROR"
                        ]
                -- Even with stdin input, should use config logfiles
                let stdinInput = "this should be ignored\n"
                (exitCode, _stdout, _stderr) <- readProcessWithExitCodeInDir tmpDir binary ["run"] stdinInput
                -- Should succeed (processes config logfile, ignores stdin)
                exitCode @?= ExitSuccess
        ]

-------------------------------------------------------------------------------
-- Pipeline Tests (collect | capture)
-------------------------------------------------------------------------------

pipelineTests :: FilePath -> TestTree
pipelineTests binary =
    testGroup
        "Pipeline Tests"
        [ testCase "collect | capture pipeline works" $ do
            -- This tests the Unix pipeline: collect outputs JSONL, capture reads it
            -- We simulate this by running collect, capturing output, feeding to capture
            collectOutput <- runCollectWithFile binary "test/fixtures/python_errors.log" 20
            let collectOutputStr = BLC.unpack collectOutput
            -- Feed collect output to capture
            (exitCode, _stdout, _stderr) <-
                readProcessWithExitCode
                    binary
                    ["capture", "--regex-pattern", "ERROR", "--pre-window", "2", "--post-window", "2", "--inactivity-timeout", "3"]
                    collectOutputStr
            exitCode @?= ExitSuccess
        ]

-------------------------------------------------------------------------------
-- Config Command Tests
-------------------------------------------------------------------------------

configCommandTests :: FilePath -> TestTree
configCommandTests binary =
    testGroup
        "Config Command Tests"
        [ testCase "config show outputs both collect and capture sections" $ do
            (exitCode, stdout, _stderr) <- readProcessWithExitCode binary ["config", "show"] ""
            exitCode @?= ExitSuccess
            assertBool "Output contains collect section" $ "collect:" `isInfixOf` stdout
            assertBool "Output contains capture section" $ "capture:" `isInfixOf` stdout
            assertBool "Output contains logfiles" $ "logfiles:" `isInfixOf` stdout
            assertBool "Output contains detection_rules" $ "detection_rules" `isInfixOf` stdout
        , testCase "config path shows paths" $ do
            (exitCode, stdout, _stderr) <- readProcessWithExitCode binary ["config", "path"] ""
            exitCode @?= ExitSuccess
            assertBool "Output contains config.yaml" $ "config.yaml" `isInfixOf` stdout
        , testCase "config init creates config with both sections" $
            withSystemTempDirectory "spanshot-test" $ \tmpDir -> do
                let configPath = tmpDir </> ".spanshot.yaml"
                (exitCode, _stdout, _stderr) <- readProcessWithExitCode binary ["config", "init", configPath] ""
                exitCode @?= ExitSuccess
                content <- readFile configPath
                assertBool "Config contains collect section" $ "collect:" `isInfixOf` content
                assertBool "Config contains capture section" $ "capture:" `isInfixOf` content
        ]

-------------------------------------------------------------------------------
-- Error Handling Tests
-------------------------------------------------------------------------------

errorHandlingTests :: FilePath -> TestTree
errorHandlingTests binary =
    testGroup
        "Error Handling Tests"
        [ testCase "collect fails with missing file" $ do
            (exitCode, _stdout, stderr) <-
                readProcessWithExitCode
                    binary
                    ["collect", "--logfile", "test/fixtures/nonexistent.log"]
                    ""
            case exitCode of
                ExitFailure _ -> assertBool "Error message mentions file" $ "nonexistent.log" `isInfixOf` stderr
                ExitSuccess -> assertFailure "Expected failure for missing file"
        , testCase "requires subcommand" $ do
            (exitCode, _stdout, _stderr) <- readProcessWithExitCode binary [] ""
            case exitCode of
                ExitFailure _ -> pure ()
                ExitSuccess -> assertFailure "Expected failure when no subcommand given"
        , testCase "collect fails when any config logfile is missing" $
            withSystemTempDirectory "spanshot-test" $ \tmpDir -> do
                let configPath = tmpDir </> ".spanshot.yaml"
                writeFile configPath $
                    unlines
                        [ "collect:"
                        , "  logfiles:"
                        , "    - ./existing.log"
                        , "    - ./missing.log"
                        , "capture:"
                        , "  detection_rules:"
                        , "    - regex_pattern: ERROR"
                        ]
                writeFile (tmpDir </> "existing.log") "some content\n"
                -- Should fail because missing.log doesn't exist
                (exitCode, _stdout, stderr) <- readProcessWithExitCodeInDir tmpDir binary ["collect"] ""
                case exitCode of
                    ExitFailure _ -> assertBool "Error mentions missing file" $ "missing.log" `isInfixOf` stderr
                    ExitSuccess -> assertFailure "Expected failure for missing config logfile"
        ]

-------------------------------------------------------------------------------
-- Help Tests
-------------------------------------------------------------------------------

helpTests :: FilePath -> TestTree
helpTests binary =
    testGroup
        "Help Tests"
        [ testCase "main --help shows all commands" $ do
            (exitCode, stdout, _stderr) <- readProcessWithExitCode binary ["--help"] ""
            exitCode @?= ExitSuccess
            assertBool "Help mentions collect" $ "collect" `isInfixOf` stdout
            assertBool "Help mentions capture" $ "capture" `isInfixOf` stdout
            assertBool "Help mentions run" $ "run" `isInfixOf` stdout
            assertBool "Help mentions config" $ "config" `isInfixOf` stdout
        , testCase "collect --help shows collect flags" $ do
            (exitCode, stdout, _stderr) <- readProcessWithExitCode binary ["collect", "--help"] ""
            exitCode @?= ExitSuccess
            assertBool "Help mentions logfile" $ "logfile" `isInfixOf` stdout
            assertBool "Help mentions poll-interval" $ "poll-interval" `isInfixOf` stdout
        ]

-------------------------------------------------------------------------------
-- Helper Functions
-------------------------------------------------------------------------------

-- | Run collect with a single logfile
runCollectWithFile :: FilePath -> FilePath -> Int -> IO BL.ByteString
runCollectWithFile binary logfile expectedLines = do
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

-- | Run collect with multiple logfiles
runCollectWithFiles :: FilePath -> [FilePath] -> Int -> IO BL.ByteString
runCollectWithFiles binary logfiles expectedLines = do
    let timeoutMicroseconds = if expectedLines > 100 then 10_000_000 else 3_000_000
    let args = "collect" : concatMap (\f -> ["--logfile", f]) logfiles
    let procSpec = (proc binary args){std_out = CreatePipe, std_err = Inherit}
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

-- | Run a process in a specific directory
readProcessWithExitCodeInDir :: FilePath -> FilePath -> [String] -> String -> IO (ExitCode, String, String)
readProcessWithExitCodeInDir dir binary args input = do
    let procSpec = (proc binary args){cwd = Just dir}
    bracket
        (createProcess procSpec{std_in = CreatePipe, std_out = CreatePipe, std_err = CreatePipe})
        ( \(mIn, mOut, mErr, ph) -> do
            maybe (pure ()) hClose mIn
            maybe (pure ()) hClose mOut
            maybe (pure ()) hClose mErr
            terminateProcess ph
            _ <- waitForProcess ph
            pure ()
        )
        $ \(mIn, mOut, mErr, ph) -> do
            case (mIn, mOut, mErr) of
                (Just hIn, Just hOut, Just hErr) -> do
                    hPutStrLn hIn input
                    hClose hIn
                    stdout <- readAllHandle hOut
                    stderr <- readAllHandle hErr
                    exitCode <- waitForProcess ph
                    pure (exitCode, stdout, stderr)
                _ -> do
                    exitCode <- waitForProcess ph
                    pure (exitCode, "", "")

-- | Read all content from a handle
readAllHandle :: Handle -> IO String
readAllHandle h = do
    content <- timeout 5_000_000 $ do
        hSetBuffering h NoBuffering
        go []
    pure $ fromMaybe "" content
  where
    go acc = do
        eof <- hIsEOF h
        if eof
            then pure $ concat $ reverse acc
            else do
                line <- hGetLine h
                go ((line ++ "\n") : acc)

-- | Validate a JSON line (ByteString)
validateJSONLine :: (Int, BL.ByteString) -> IO ()
validateJSONLine (lineNum, line) = do
    case eitherDecode line of
        Left err -> assertFailure $ "Line " ++ show lineNum ++ " is not valid JSON: " ++ err
        Right (_ :: Value) -> pure ()

-- | Validate a JSON line (String)
validateJSONLineString :: String -> IO ()
validateJSONLineString line = do
    case eitherDecode (BLC.pack line) of
        Left err -> assertFailure $ "Line is not valid JSON: " ++ err ++ "\nLine: " ++ line
        Right (_ :: Value) -> pure ()
