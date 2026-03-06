module Main where

import Capture (captureFromStream, captureFromStreamWithTicks)
import Collect (collectFromFileOnce, collectFromFileTail, collectFromFileWithCleanup)
import Config (ConfigPathInfo (..), ConfigPaths (..), ConfigWarning (..), InitConfigError (..), capture, getConfigPath, getConfigPaths, getProjectConfigPath, initConfigFile, loadConfig, toCaptureOptions)
import Control.Exception (IOException, catch)
import Data.Aeson qualified as Aeson
import Data.ByteString.Char8 qualified as BS
import Data.ByteString.Lazy.Char8 qualified as BL

import Control.Monad (when)
import Data.Yaml qualified as Yaml
import OptEnvConf (
    HasParser (settingsParser),
    argument,
    auto,
    command,
    commands,
    help,
    long,
    metavar,
    name,
    optional,
    reader,
    runSettingsParser,
    setting,
    short,
    some,
    str,
    switch,
    value,
    withoutConfig,
 )
import Paths_hs_spanshot (version)
import Streaming.Prelude qualified as S
import System.Directory (doesDirectoryExist, getCurrentDirectory)
import System.Exit (exitFailure, exitWith)
import System.FilePath ((</>))
import System.IO (hFlush, hPutStrLn, stderr, stdout)
import System.IO.Error (isDoesNotExistError, isPermissionError)
import Types (CollectEvent, DetectionRule (..), SpanShot, defaultCollectOptions, defaultMaxPostWindowEvents, mkCaptureOptions)
import Wrap qualified

newtype Instructions = Instructions Dispatch
    deriving (Show)

instance HasParser Instructions where
    settingsParser = Instructions <$> settingsParser

data Dispatch
    = DispatchCollect CollectSettings
    | DispatchConfig ConfigCommand
    | DispatchCapture CaptureSettings
    | DispatchRun RunSettings
    | DispatchWrap WrapSettings
    deriving (Show)

instance HasParser Dispatch where
    settingsParser =
        commands
            [ command "collect" "Collect logs from a file and output JSONL events" $
                DispatchCollect <$> settingsParser
            , command "config" "Manage configuration" $
                DispatchConfig <$> settingsParser
            , command "capture" "Capture errors with context from a log file and output SpanShots as JSONL" $
                DispatchCapture <$> settingsParser
            , command "run" "Monitor a log file continuously, capturing errors with context as JSONL" $
                DispatchRun <$> settingsParser
            , command "wrap" "Run a command with SpanShot monitoring, preserving exit code" $
                DispatchWrap <$> settingsParser
            ]

data ConfigCommand
    = ConfigShow
    | ConfigPath
    | ConfigInit ConfigInitSettings
    | ConfigValidate
    deriving (Show)

instance HasParser ConfigCommand where
    settingsParser =
        commands
            [ command "show" "Show current configuration" $ pure ConfigShow
            , command "path" "Show configuration file path" $ pure ConfigPath
            , command "init" "Initialize a new config file" $ ConfigInit <$> settingsParser
            , command "validate" "Validate configuration files without running" $ pure ConfigValidate
            ]

data ConfigInitSettings = ConfigInitSettings
    { initPath :: Maybe FilePath
    , initForce :: Bool
    , initUser :: Bool
    }
    deriving (Show)

instance HasParser ConfigInitSettings where
    settingsParser =
        ConfigInitSettings
            <$> optional
                ( withoutConfig
                    ( setting
                        [ help "Path where to create the config file (default: current directory)"
                        , reader str
                        , argument
                        , metavar "PATH"
                        ]
                    )
                )
            <*> withoutConfig
                ( setting
                    [ help "Overwrite existing config file"
                    , switch True
                    , long "force"
                    , short 'f'
                    , value False
                    ]
                )
            <*> withoutConfig
                ( setting
                    [ help "Create user config at ~/.config/spanshot/config.yaml instead of project config"
                    , switch True
                    , long "user"
                    , short 'u'
                    , value False
                    ]
                )

newtype CollectSettings = CollectSettings
    { collectLogfile :: FilePath
    }
    deriving (Show)

instance HasParser CollectSettings where
    settingsParser =
        CollectSettings
            <$> withoutConfig
                ( setting
                    [ help "Path to the logfile to tail"
                    , reader str
                    , name "logfile"
                    , metavar "PATH"
                    ]
                )

-- | Settings for the 'capture' command (User Story 1)
data CaptureSettings = CaptureSettings
    { captureLogfile :: FilePath
    , capturePattern :: String
    , capturePreWindow :: Int
    , capturePostWindow :: Int
    , captureVerbose :: Bool
    }
    deriving (Show)

instance HasParser CaptureSettings where
    settingsParser =
        CaptureSettings
            <$> withoutConfig
                ( setting
                    [ help "Path to the log file to process"
                    , reader str
                    , name "logfile"
                    , metavar "PATH"
                    ]
                )
            <*> withoutConfig
                ( setting
                    [ help "Regex pattern to detect errors (e.g., \"ERROR|FATAL\")"
                    , reader str
                    , name "regex-pattern"
                    , metavar "PATTERN"
                    ]
                )
            <*> withoutConfig
                ( setting
                    [ help "Pre-window duration in seconds (context before error)"
                    , reader auto
                    , name "pre-window"
                    , metavar "SECONDS"
                    , value 5
                    ]
                )
            <*> withoutConfig
                ( setting
                    [ help "Post-window duration in seconds (context after error)"
                    , reader auto
                    , name "post-window"
                    , metavar "SECONDS"
                    , value 5
                    ]
                )
            <*> withoutConfig
                ( setting
                    [ help "Enable verbose progress output to stderr"
                    , switch True
                    , long "verbose"
                    , short 'v'
                    , value False
                    ]
                )

-- | Settings for the 'run' command (User Story 2)
data RunSettings = RunSettings
    { runLogfile :: FilePath
    , runVerbose :: Bool
    }
    deriving (Show)

instance HasParser RunSettings where
    settingsParser =
        RunSettings
            <$> withoutConfig
                ( setting
                    [ help "Path to the log file to monitor continuously"
                    , reader str
                    , name "logfile"
                    , metavar "PATH"
                    ]
                )
            <*> withoutConfig
                ( setting
                    [ help "Enable verbose progress output to stderr"
                    , switch True
                    , long "verbose"
                    , short 'v'
                    , value False
                    ]
                )

-- | Settings for the 'wrap' command (User Story 2 - Wrap Mode)
data WrapSettings = WrapSettings
    { wrapCommand :: [String]
    -- ^ Command and arguments to run (everything after --)
    }
    deriving (Show)

instance HasParser WrapSettings where
    settingsParser =
        WrapSettings
            <$> some
                ( withoutConfig
                    ( setting
                        [ help "Command to run (after --)"
                        , reader str
                        , argument
                        , metavar "COMMAND"
                        ]
                    )
                )

main :: IO ()
main = do
    Instructions dispatch <-
        runSettingsParser
            version
            "SpanShot - Log collector and analyzer"
    case dispatch of
        DispatchCollect (CollectSettings logfilePath) ->
            runCollect logfilePath `catch` handleIOError logfilePath
        DispatchConfig cmd ->
            runConfig cmd
        DispatchCapture settings ->
            runCapture settings `catch` handleIOError (captureLogfile settings)
        DispatchRun settings ->
            runRun settings `catch` handleIOError (runLogfile settings)
        DispatchWrap settings ->
            runWrapCommand settings

-- | Run the capture command: process a log file and output SpanShots as JSONL
runCapture :: CaptureSettings -> IO ()
runCapture settings = do
    let logfilePath = captureLogfile settings
    let regexPat = capturePattern settings
    let preWin = fromIntegral (capturePreWindow settings)
    let postWin = fromIntegral (capturePostWindow settings)
    let verbose = captureVerbose settings
    let rules = [RegexRule regexPat]

    -- Validate arguments and create CaptureOptions
    case mkCaptureOptions preWin postWin 10 defaultMaxPostWindowEvents rules of
        Left err -> do
            hPutStrLn stderr $ "Error: Invalid capture options: " ++ err
            exitFailure
        Right captureOpts -> do
            when verbose $
                hPutStrLn stderr $
                    "[spanshot] Processing " ++ logfilePath
            -- Use collectFromFileOnce for one-shot capture (not tailing)
            collectFromFileOnce logfilePath $ \events -> do
                let spanshots = captureFromStream captureOpts events
                S.mapM_ printSpanShot spanshots
            when verbose $
                hPutStrLn stderr "[spanshot] Done"

-- | Run the run command: continuously monitor a log file and output SpanShots as JSONL
runRun :: RunSettings -> IO ()
runRun settings = do
    let logfilePath = runLogfile settings
    let verbose = runVerbose settings

    -- Load config and extract capture options
    (config, warnings) <- loadConfig
    printConfigWarnings warnings

    case toCaptureOptions (capture config) of
        Left err -> do
            hPutStrLn stderr $ "Error: Invalid configuration: " ++ err
            exitFailure
        Right captureOpts -> do
            when verbose $
                hPutStrLn stderr $
                    "[spanshot] Monitoring " ++ logfilePath ++ " (starting from end)"
            -- Use collectFromFileTail to start from end of file (like tail -f)
            -- Use captureFromStreamWithTicks to emit SpanShots even when no events arrive
            let tickIntervalMicros = 1000000 -- 1 second
            collectFromFileTail defaultCollectOptions logfilePath $ \events -> do
                let spanshots = captureFromStreamWithTicks captureOpts tickIntervalMicros events
                S.mapM_ printSpanShot spanshots
            when verbose $
                hPutStrLn stderr "[spanshot] Done"

-- | Run the wrap command: run a command with SpanShot monitoring
runWrapCommand :: WrapSettings -> IO ()
runWrapCommand settings = do
    case wrapCommand settings of
        [] -> do
            hPutStrLn stderr "Error: No command specified. Usage: spanshot wrap -- COMMAND [ARGS...]"
            exitFailure
        (cmd : args) -> do
            result <- Wrap.runWrap cmd args
            exitWith (Wrap.wrapExitCode result)

runCollect :: FilePath -> IO ()
runCollect logfilePath = do
    (config, warnings) <- loadConfig
    -- Print any config warnings to stderr
    printConfigWarnings warnings
    -- Validate config (including regex patterns) before starting collection
    case toCaptureOptions (capture config) of
        Left err -> do
            hPutStrLn stderr $ "Error: Invalid configuration: " ++ err
            exitFailure
        Right _captureOpts ->
            -- TODO: Use captureOpts when capture processing is integrated
            collectFromFileWithCleanup defaultCollectOptions logfilePath $ \events ->
                S.mapM_ printEvent events

runConfig :: ConfigCommand -> IO ()
runConfig ConfigShow = do
    (config, warnings) <- loadConfig
    -- Print any config warnings to stderr
    printConfigWarnings warnings
    BS.putStrLn $ Yaml.encode config
runConfig ConfigPath = do
    cwd <- getCurrentDirectory
    paths <- getConfigPaths cwd
    -- Print user config path with status
    printPathInfo "user" (cpiUser paths)
    -- Print project config path with status (if in a project)
    case cpiProject paths of
        Nothing -> putStrLn "project: (not in a git project)"
        Just projInfo -> printPathInfo "project" projInfo
runConfig ConfigValidate = do
    (config, warnings) <- loadConfig
    -- Print errors for any config file issues (parse or validation)
    let hasErrors = not (null warnings)
    mapM_ (printWarning "Error") warnings
    -- If config files failed, exit with failure
    when hasErrors $ do
        hPutStrLn stderr $ colorRed ++ "Config validation failed." ++ colorReset
        exitFailure
    -- Validate the loaded config values
    case toCaptureOptions (capture config) of
        Left err -> do
            hPutStrLn stderr $ colorRed ++ "Error" ++ colorReset ++ ": Invalid configuration: " ++ err
            exitFailure
        Right _ -> putStrLn "Configuration is valid."
runConfig (ConfigInit settings) = do
    targetPath <-
        if initUser settings
            then getConfigPath
            else case initPath settings of
                -- Explicit path provided - use it directly
                Just path -> pure path
                -- No path provided - find project root
                Nothing -> do
                    cwd <- getCurrentDirectory
                    projectPath <- getProjectConfigPath cwd
                    case projectPath of
                        Just p -> pure p
                        -- Not in a project, use cwd
                        Nothing -> pure $ cwd </> ".spanshot.yaml"
    result <- initConfigFile targetPath (initForce settings)
    case result of
        Left (ConfigFileExists path) -> do
            hPutStrLn stderr $ "Error: Config file already exists: " ++ path
            hPutStrLn stderr "Use --force to overwrite."
            exitFailure
        Left (InitIOError path err) -> do
            hPutStrLn stderr $ "Error: Failed to create config at " ++ path ++ ": " ++ err
            exitFailure
        Right () -> do
            -- Determine the actual file path for the message
            -- If targetPath is a directory, initConfigFile creates .spanshot.yaml inside
            isDir <- doesDirectoryExist targetPath
            let actualPath =
                    if isDir
                        then targetPath </> ".spanshot.yaml"
                        else targetPath
            putStrLn $ "Created config file: " ++ actualPath

-- | Print a config path with existence indicator
printPathInfo :: String -> ConfigPathInfo -> IO ()
printPathInfo label info = do
    let status = if cpiExists info then "[found]" else "[not found]"
    putStrLn $ label ++ ": " ++ cpiPath info ++ " " ++ status

-- | Print config warnings to stderr
printConfigWarnings :: [ConfigWarning] -> IO ()
printConfigWarnings [] = pure ()
printConfigWarnings warnings = do
    hPutStrLn stderr ""
    mapM_ (printWarning "Warning") warnings
    hPutStrLn stderr $ colorYellow ++ "Using default configuration." ++ colorReset
    hPutStrLn stderr ""

-- | Print a config warning with a given prefix (e.g., "Warning" or "Error")
printWarning :: String -> ConfigWarning -> IO ()
printWarning prefix (ConfigParseWarning path err) = do
    hPutStrLn stderr $ coloredPrefix prefix ++ ": Failed to parse " ++ path
    mapM_ (hPutStrLn stderr . ("  " ++)) (lines err)
printWarning prefix (ConfigValidationWarning path err) = do
    hPutStrLn stderr $ coloredPrefix prefix ++ ": Invalid config in " ++ path
    hPutStrLn stderr $ "  " ++ err

-- | ANSI color codes
colorRed, colorYellow, colorReset :: String
colorRed = "\ESC[31m"
colorYellow = "\ESC[33m"
colorReset = "\ESC[0m"

-- | Apply color to prefix based on severity
coloredPrefix :: String -> String
coloredPrefix "Error" = colorRed ++ "Error" ++ colorReset
coloredPrefix "Warning" = colorYellow ++ "Warning" ++ colorReset
coloredPrefix other = other

handleIOError :: FilePath -> IOException -> IO ()
handleIOError path e
    | isDoesNotExistError e = do
        hPutStrLn stderr $ "Error: File not found: " ++ path
        exitFailure
    | isPermissionError e = do
        hPutStrLn stderr $ "Error: Permission denied: " ++ path
        exitFailure
    | otherwise = do
        hPutStrLn stderr $ "Error reading file '" ++ path ++ "': " ++ show e
        exitFailure

-- | Print any JSON-serializable value as JSONL (one line, flushed to stdout)
printJsonLn :: (Aeson.ToJSON a) => a -> IO ()
printJsonLn x = do
    BL.putStrLn $ Aeson.encode x
    hFlush stdout

printEvent :: CollectEvent -> IO ()
printEvent = printJsonLn

-- | Print a SpanShot as JSONL (one line per SpanShot)
printSpanShot :: SpanShot -> IO ()
printSpanShot = printJsonLn
