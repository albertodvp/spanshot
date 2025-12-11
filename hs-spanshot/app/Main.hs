module Main where

import Capture (captureFromCaptureInput, withInactivityTimeout)
import Collect (collectFromFileWithCleanup, collectFromJSONLStdin, collectFromStdin)
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
    loadEffectiveConfig,
    resolveLogfiles,
    toCaptureOptions,
 )
import Control.Exception (IOException)
import Data.Aeson qualified as Aeson
import Data.ByteString.Char8 qualified as BS
import Data.ByteString.Lazy.Char8 qualified as BL
import Data.Time (NominalDiffTime)
import Data.Yaml qualified as Yaml

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
import System.Directory (doesDirectoryExist, doesFileExist, getCurrentDirectory)
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
    | DispatchCapture CaptureOnlySettings
    | DispatchRun RunSettings
    deriving (Show)

-- | Parser for dispatch commands (not using HasParser to avoid config loading issues)
dispatchParser :: Parser Dispatch
dispatchParser =
    commands
        [ command "collect" "Collect logs from files or stdin and output JSONL events" $
            DispatchCollect <$> settingsParser
        , command "config" "Manage configuration" $
            DispatchConfig <$> settingsParser
        , command "capture" "Capture error spans from JSONL input on stdin" $
            DispatchCapture <$> captureOnlySettingsParser
        , command "run" "Collect and capture errors from log files (full pipeline)" $
            DispatchRun <$> runSettingsParser_
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

{- | Settings for collect command.
Supports multiple logfiles via CLI --logfile flags, config file logfiles,
or stdin when no logfiles are specified.
-}
data CollectSettings = CollectSettings
    { collectLogfiles :: [FilePath]
    -- ^ Logfiles from CLI (overrides config)
    , collectConfigLogfiles :: [FilePath]
    -- ^ Logfiles from config file
    , collectPollIntervalMs :: Int
    -- ^ Poll interval in milliseconds
    }
    deriving (Show)

instance HasParser CollectSettings where
    settingsParser =
        CollectSettings
            -- CLI logfiles (can specify multiple)
            <$> many
                ( withoutConfig
                    ( setting
                        [ help "Path to the logfile to tail (can be specified multiple times)"
                        , reader str
                        , long "logfile"
                        , option
                        , metavar "PATH"
                        ]
                    )
                )
            -- Config file logfiles (from collect.logfiles)
            <*> subConfig_ "collect" collectConfigLogfilesParser
            -- Poll interval (CLI or config)
            <*> subConfig_
                "collect"
                ( setting
                    [ help "Poll interval in milliseconds"
                    , reader auto
                    , long "poll-interval"
                    , option
                    , metavar "MS"
                    , conf "poll_interval_ms"
                    , value defaultPollIntervalMs
                    ]
                )

-- | Parser for logfiles from config file
collectConfigLogfilesParser :: Parser [FilePath]
collectConfigLogfilesParser =
    setting
        [ help "Logfiles from config file"
        , conf "logfiles"
        , value []
        ]

-- | Default poll interval
defaultPollIntervalMs :: Int
defaultPollIntervalMs = 150

{- | Settings for capture command (stdin-only).
These can come from CLI arguments, environment variables, or config files.
opt-env-conf handles the precedence: CLI > env > config > defaults
Note: capture command does NOT have --logfile, it reads JSONL from stdin.
-}
newtype CaptureOnlySettings = CaptureOnlySettings
    { captureOnlyConfig :: CaptureConfig
    }
    deriving (Show)

-- | Parser for capture-only settings (stdin-only, no logfile)
captureOnlySettingsParser :: Parser CaptureOnlySettings
captureOnlySettingsParser =
    CaptureOnlySettings
        <$> subConfig_ "capture" captureConfigParser

{- | Settings for run command (full pipeline).
Combines collect settings (logfiles) with capture settings.
-}
data RunSettings = RunSettings
    { runLogfiles :: [FilePath]
    -- ^ Logfiles from CLI (overrides config)
    , runConfigLogfiles :: [FilePath]
    -- ^ Logfiles from config file
    , runPollIntervalMs :: Int
    -- ^ Poll interval in milliseconds
    , runCaptureConfig :: CaptureConfig
    -- ^ Capture configuration
    }
    deriving (Show)

-- | Parser for run settings (full pipeline with all options)
runSettingsParser_ :: Parser RunSettings
runSettingsParser_ =
    RunSettings
        -- CLI logfiles (can specify multiple)
        <$> many
            ( withoutConfig
                ( setting
                    [ help "Path to the logfile to process (can be specified multiple times)"
                    , reader str
                    , long "logfile"
                    , option
                    , metavar "PATH"
                    ]
                )
            )
        -- Config file logfiles (from collect.logfiles)
        <*> subConfig_ "collect" collectConfigLogfilesParser
        -- Poll interval (CLI or config)
        <*> subConfig_
            "collect"
            ( setting
                [ help "Poll interval in milliseconds"
                , reader auto
                , long "poll-interval"
                , option
                , metavar "MS"
                , conf "poll_interval_ms"
                , value defaultPollIntervalMs
                ]
            )
        -- Capture config
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
        DispatchCollect settings ->
            runCollect settings
        DispatchConfig cmd ->
            runConfig cmd
        DispatchCapture settings ->
            runCaptureStdin settings
        DispatchRun settings ->
            runFullPipeline settings

{- | Determine effective logfiles from CLI and config
CLI logfiles override config logfiles entirely
-}
effectiveLogfiles :: [FilePath] -> [FilePath] -> [FilePath]
effectiveLogfiles cliLogfiles configLogfiles
    | not (null cliLogfiles) = cliLogfiles
    | otherwise = configLogfiles

runCollect :: CollectSettings -> IO ()
runCollect settings = do
    -- If CLI logfiles provided, use them directly
    -- If using config logfiles, resolve relative paths relative to config file
    logfiles <-
        if not (null (collectLogfiles settings))
            then pure (collectLogfiles settings)
            else do
                -- Get config file path for relative path resolution
                (_, _, mConfigPath) <- loadEffectiveConfig
                resolveLogfiles mConfigPath (collectConfigLogfiles settings)
    if null logfiles
        then -- Read from stdin
            runCollectStdin
        else do
            -- Check that all files exist
            mapM_ checkFileExists logfiles
            -- Process each file
            mapM_ runCollectFile logfiles

runCollectStdin :: IO ()
runCollectStdin = do
    collectFromStdin $ \events ->
        S.mapM_ printEvent events

runCollectFile :: FilePath -> IO ()
runCollectFile logfilePath = do
    collectFromFileWithCleanup defaultCollectOptions logfilePath $ \events ->
        S.mapM_ printEvent events

checkFileExists :: FilePath -> IO ()
checkFileExists path = do
    exists <- doesFileExist path
    if exists
        then pure ()
        else do
            hPutStrLn stderr $ "Error: File not found: " ++ path
            exitFailure

runConfig :: ConfigCommand -> IO ()
runConfig ConfigShow = do
    -- Load and display the effective configuration
    (effectiveConfig, loadedPaths, _configDir) <- loadEffectiveConfig
    if null loadedPaths
        then putStrLn "# No config files found, showing defaults"
        else putStrLn $ "# Loaded from: " ++ unwords loadedPaths
    -- Output as valid YAML
    BS.putStr $ Yaml.encode effectiveConfig
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

-- | Run capture reading JSONL CollectEvents from stdin
runCaptureStdin :: CaptureOnlySettings -> IO ()
runCaptureStdin settings = do
    -- Validate config
    case toCaptureOptions (captureOnlyConfig settings) of
        Left err -> do
            hPutStrLn stderr $ "Error: Invalid configuration: " ++ err
            exitFailure
        Right opts -> do
            -- Read JSONL CollectEvents from stdin and process them
            collectFromJSONLStdin $ \events ->
                let timedEvents = withInactivityTimeout (inactivityTimeout opts) events
                 in S.mapM_ printSpanShot $ captureFromCaptureInput opts timedEvents

-- | Full pipeline (collect + capture combined)
runFullPipeline :: RunSettings -> IO ()
runFullPipeline settings = do
    -- If CLI logfiles provided, use them directly
    -- If using config logfiles, resolve relative paths relative to config file
    logfiles <-
        if not (null (runLogfiles settings))
            then pure (runLogfiles settings)
            else do
                -- Get config file path for relative path resolution
                (_, _, mConfigPath) <- loadEffectiveConfig
                resolveLogfiles mConfigPath (runConfigLogfiles settings)
    -- Validate capture config first
    case toCaptureOptions (runCaptureConfig settings) of
        Left err -> do
            hPutStrLn stderr $ "Error: Invalid configuration: " ++ err
            exitFailure
        Right opts -> do
            if null logfiles
                then do
                    -- Read from stdin
                    collectFromStdin $ \events ->
                        let timedEvents = withInactivityTimeout (inactivityTimeout opts) events
                         in S.mapM_ printSpanShot $ captureFromCaptureInput opts timedEvents
                else do
                    -- Check that all files exist
                    mapM_ checkFileExists logfiles
                    -- Process files (for now, just the first one - multi-file merge is TODO)
                    case logfiles of
                        [] -> pure () -- Should not happen due to null check above
                        (firstFile : _) ->
                            collectFromFileWithCleanup defaultCollectOptions firstFile $ \events ->
                                let timedEvents = withInactivityTimeout (inactivityTimeout opts) events
                                 in S.mapM_ printSpanShot $ captureFromCaptureInput opts timedEvents

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
