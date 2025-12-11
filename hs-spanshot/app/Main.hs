module Main where

import Capture (captureFromCaptureInput, withInactivityTimeout)
import Collect (collectFromFileWithCleanup)
import Config (
    CaptureConfig (..),
    ConfigPathInfo (..),
    ConfigPaths (..),
    InitConfigError (..),
    defaultCaptureConfig,
    getConfigFilePaths,
    getConfigPath,
    getConfigPaths,
    getProjectConfigPath,
    initConfigFile,
    toCaptureOptions,
 )
import Control.Exception (IOException, catch)
import Data.Aeson qualified as Aeson
import Data.ByteString.Lazy.Char8 qualified as BL
import Data.Time (NominalDiffTime)

import OptEnvConf (
    HasParser (settingsParser),
    Parser,
    Reader,
    argument,
    auto,
    command,
    commands,
    conf,
    eitherReader,
    help,
    long,
    many,
    mapIO,
    metavar,
    name,
    option,
    optional,
    reader,
    runSettingsParser,
    setting,
    short,
    str,
    subConfig_,
    switch,
    value,
    withCombinedYamlConfigs,
    withoutConfig,
 )
import Path (Abs, File, Path)
import Paths_hs_spanshot (version)
import Streaming.Prelude qualified as S
import System.Directory (doesDirectoryExist, getCurrentDirectory)
import System.Exit (exitFailure)
import System.FilePath ((</>))
import System.IO (hFlush, hPutStrLn, stderr, stdout)
import System.IO.Error (isDoesNotExistError, isPermissionError)
import Types (
    CaptureOptions (inactivityTimeout),
    CollectEvent,
    DetectionRule (..),
    SpanShot,
    defaultCollectOptions,
 )

newtype Instructions = Instructions Dispatch
    deriving (Show)

instance HasParser Instructions where
    settingsParser =
        withCombinedYamlConfigs configFilePathsParser $
            Instructions <$> dispatchParser

data Dispatch
    = DispatchCollect CollectSettings
    | DispatchConfig ConfigCommand
    | DispatchCapture CaptureSettings
    | DispatchRun RunSettings
    deriving (Show)

-- | Parser for dispatch commands (not using HasParser to avoid config loading issues)
dispatchParser :: Parser Dispatch
dispatchParser =
    commands
        [ command "collect" "Collect logs from a file and output JSONL events" $
            DispatchCollect <$> settingsParser
        , command "config" "Manage configuration" $
            DispatchConfig <$> settingsParser
        , command "capture" "Capture error spans from a log file" $
            DispatchCapture <$> captureSettingsParser
        , command "run" "Collect and capture errors from a log file (full pipeline)" $
            DispatchRun <$> captureSettingsParser
        ]

data ConfigCommand
    = ConfigShow
    | ConfigPath
    | ConfigInit ConfigInitSettings
    deriving (Show)

instance HasParser ConfigCommand where
    settingsParser =
        commands
            [ command "show" "Show current configuration" $ pure ConfigShow
            , command "path" "Show configuration file path" $ pure ConfigPath
            , command "init" "Initialize a new config file" $ ConfigInit <$> settingsParser
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

{- | Settings for capture and run commands.
These can come from CLI arguments, environment variables, or config files.
opt-env-conf handles the precedence: CLI > env > config > defaults
-}
data CaptureSettings = CaptureSettings
    { captureLogfile :: FilePath
    , captureConfig :: CaptureConfig
    }
    deriving (Show)

-- | Parser for capture settings (not using HasParser class)
captureSettingsParser :: Parser CaptureSettings
captureSettingsParser =
    CaptureSettings
        -- Logfile is CLI-only (doesn't make sense in config)
        <$> withoutConfig
            ( setting
                [ help "Path to the logfile to process"
                , reader str
                , name "logfile"
                , metavar "PATH"
                ]
            )
        -- Capture config can come from CLI, env, or config file
        -- Use subConfig_ to nest under "capture:" in config files
        <*> subConfig_ "capture" captureConfigParser

{- | Parser for capture configuration.
Each setting can come from CLI args, env vars, or config file.
-}
captureConfigParser :: Parser CaptureConfig
captureConfigParser =
    mkCaptureConfig
        <$> setting
            [ help "Pre-window duration in seconds (context before error)"
            , reader secondsReader
            , option
            , long "pre-window"
            , metavar "SECONDS"
            , conf "pre_window_duration"
            , value (ccPreWindowDuration defaultCaptureConfig)
            ]
        <*> setting
            [ help "Post-window duration in seconds (context after error)"
            , reader secondsReader
            , option
            , long "post-window"
            , metavar "SECONDS"
            , conf "post_window_duration"
            , value (ccPostWindowDuration defaultCaptureConfig)
            ]
        <*> setting
            [ help "Minimum number of context events to capture"
            , reader auto
            , option
            , long "min-context"
            , metavar "COUNT"
            , conf "min_context_events"
            , value (ccMinContextEvents defaultCaptureConfig)
            ]
        <*> setting
            [ help "Inactivity timeout in seconds (for flushing pending captures)"
            , reader secondsReader
            , option
            , long "inactivity-timeout"
            , metavar "SECONDS"
            , conf "inactivity_timeout"
            , value (ccInactivityTimeout defaultCaptureConfig)
            ]
        -- Detection rules: CLI patterns override config file rules entirely
        <*> many
            ( withoutConfig $
                setting
                    [ help "Regex pattern to detect errors (can be specified multiple times)"
                    , reader str
                    , long "regex-pattern"
                    , short 'p'
                    , option
                    , metavar "PATTERN"
                    ]
            )
        -- Config file detection rules (used when no CLI patterns provided)
        <*> setting
            [ help "Detection rules from config file"
            , conf "detection_rules"
            , value (ccDetectionRules defaultCaptureConfig)
            ]
  where
    -- Helper to combine CLI patterns with config rules
    mkCaptureConfig preWindow postWindow minContext inactivityTimeoutVal cliPatterns configRules =
        CaptureConfig
            { ccPreWindowDuration = preWindow
            , ccPostWindowDuration = postWindow
            , ccMinContextEvents = minContext
            , ccDetectionRules = selectRules cliPatterns configRules
            , ccInactivityTimeout = inactivityTimeoutVal
            }
    -- If CLI patterns provided, use them; otherwise use config file rules
    selectRules [] configRules = configRules
    selectRules patterns _ = map RegexRule patterns

-- | RunSettings is the same as CaptureSettings (full pipeline uses same options)
type RunSettings = CaptureSettings

-- | Parser for loading config files
configFilePathsParser :: Parser [Path Abs File]
configFilePathsParser = mapIO (const getConfigFilePaths) (pure ())

-- | Reader for NominalDiffTime from seconds (as an integer or decimal)
secondsReader :: Reader NominalDiffTime
secondsReader = eitherReader $ \s ->
    case reads s of
        [(n, "")] -> Right (realToFrac (n :: Double))
        _ -> Left $ "Invalid duration: " ++ s ++ " (expected number of seconds)"

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
            runFullPipeline settings `catch` handleIOError (captureLogfile settings)

runCollect :: FilePath -> IO ()
runCollect logfilePath = do
    -- For collect, we just stream events - no capture config needed
    collectFromFileWithCleanup defaultCollectOptions logfilePath $ \events ->
        S.mapM_ printEvent events

runConfig :: ConfigCommand -> IO ()
runConfig ConfigShow = do
    -- Load and display the effective configuration
    configPaths <- getConfigFilePaths
    if null configPaths
        then do
            putStrLn "# No config files found, showing defaults:"
            printCaptureConfig defaultCaptureConfig
        else do
            putStrLn $ "# Config files: " ++ show (map show configPaths)
            putStrLn "# Use 'spanshot capture --help' to see effective values"
            printCaptureConfig defaultCaptureConfig
runConfig ConfigPath = do
    cwd <- getCurrentDirectory
    paths <- getConfigPaths cwd
    -- Print user config path with status
    printPathInfo "user" (cpiUser paths)
    -- Print project config path with status (if in a project)
    case cpiProject paths of
        Nothing -> putStrLn "project: (not in a git project)"
        Just projInfo -> printPathInfo "project" projInfo
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

-- | Print capture config in a readable format
printCaptureConfig :: CaptureConfig -> IO ()
printCaptureConfig cc = do
    putStrLn "capture:"
    putStrLn $ "  pre_window_duration: " ++ show (ccPreWindowDuration cc)
    putStrLn $ "  post_window_duration: " ++ show (ccPostWindowDuration cc)
    putStrLn $ "  min_context_events: " ++ show (ccMinContextEvents cc)
    putStrLn $ "  inactivity_timeout: " ++ show (ccInactivityTimeout cc)
    putStrLn "  detection_rules:"
    mapM_ (\(RegexRule p) -> putStrLn $ "    - regex_pattern: \"" ++ p ++ "\"") (ccDetectionRules cc)

runCapture :: CaptureSettings -> IO ()
runCapture settings = do
    -- Validate config and run capture
    case toCaptureOptions (captureConfig settings) of
        Left err -> do
            hPutStrLn stderr $ "Error: Invalid configuration: " ++ err
            exitFailure
        Right opts ->
            collectFromFileWithCleanup defaultCollectOptions (captureLogfile settings) $ \events ->
                let timedEvents = withInactivityTimeout (inactivityTimeout opts) events
                 in S.mapM_ printSpanShot $ captureFromCaptureInput opts timedEvents

-- | Full pipeline (collect + capture combined) - same implementation as capture
runFullPipeline :: RunSettings -> IO ()
runFullPipeline = runCapture

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

printEvent :: CollectEvent -> IO ()
printEvent event = do
    BL.putStrLn $ Aeson.encode event
    hFlush stdout

printSpanShot :: SpanShot -> IO ()
printSpanShot shot = do
    BL.putStrLn $ Aeson.encode shot
    hFlush stdout
