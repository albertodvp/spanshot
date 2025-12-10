module Main where

import Capture (captureFromStream)
import Collect (collectFromFileWithCleanup)
import Config (ConfigPathInfo (..), ConfigPaths (..), ConfigWarning (..), InitConfigError (..), capture, getConfigPath, getConfigPaths, getProjectConfigPath, initConfigFile, loadConfig, toCaptureOptions)
import Control.Exception (IOException, catch)
import Data.Aeson qualified as Aeson
import Data.ByteString.Char8 qualified as BS
import Data.ByteString.Lazy.Char8 qualified as BL

import Data.Yaml qualified as Yaml
import OptEnvConf (
    HasParser (settingsParser),
    argument,
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
    str,
    switch,
    value,
    withoutConfig,
 )
import Paths_hs_spanshot (version)
import Streaming.Prelude qualified as S
import System.Directory (doesDirectoryExist, getCurrentDirectory)
import System.Exit (exitFailure)
import System.FilePath ((</>))
import System.IO (hFlush, hPutStrLn, stderr, stdout)
import System.IO.Error (isDoesNotExistError, isPermissionError)
import Types (
    CollectEvent,
    SpanShot,
    defaultCollectOptions,
 )

newtype Instructions = Instructions Dispatch
    deriving (Show)

instance HasParser Instructions where
    settingsParser = Instructions <$> settingsParser

data Dispatch
    = DispatchCollect CollectSettings
    | DispatchConfig ConfigCommand
    | DispatchCapture CaptureSettings
    | DispatchRun RunSettings
    deriving (Show)

instance HasParser Dispatch where
    settingsParser =
        commands
            [ command "collect" "Collect logs from a file and output JSONL events" $
                DispatchCollect <$> settingsParser
            , command "config" "Manage configuration" $
                DispatchConfig <$> settingsParser
            , command "capture" "Capture error spans from a log file" $
                DispatchCapture <$> settingsParser
            , command "run" "Collect and capture errors from a log file (full pipeline)" $
                DispatchRun <$> settingsParser
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

{- | Settings for capture and run commands
These are CLI overrides - config file values are used as defaults
-}
data CaptureSettings = CaptureSettings
    { captureLogfile :: FilePath
    }
    deriving (Show)

instance HasParser CaptureSettings where
    settingsParser =
        CaptureSettings
            <$> withoutConfig
                ( setting
                    [ help "Path to the logfile to process"
                    , reader str
                    , name "logfile"
                    , metavar "PATH"
                    ]
                )

-- | RunSettings is the same as CaptureSettings (full pipeline uses same options)
type RunSettings = CaptureSettings

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
    mapM_ printWarning warnings
    hPutStrLn stderr "Using default configuration for failed config files."
  where
    printWarning (ConfigParseWarning path err) =
        hPutStrLn stderr $ "Warning: Failed to parse config file " ++ path ++ ": " ++ err
    printWarning (ConfigValidationWarning path err) =
        hPutStrLn stderr $ "Warning: Invalid configuration in " ++ path ++ ": " ++ err

runCapture :: CaptureSettings -> IO ()
runCapture settings = do
    (config, warnings) <- loadConfig
    -- Print any config warnings to stderr
    printConfigWarnings warnings
    -- Validate config and run capture
    case toCaptureOptions (capture config) of
        Left err -> do
            hPutStrLn stderr $ "Error: Invalid configuration: " ++ err
            exitFailure
        Right opts ->
            collectFromFileWithCleanup defaultCollectOptions (captureLogfile settings) $ \events ->
                S.mapM_ printSpanShot $ captureFromStream opts events

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
