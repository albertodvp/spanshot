module Main where

import Collect (collectFromFileWithCleanup)
import Config (ConfigPathInfo (..), ConfigPaths (..), InitConfigError (..), capture, getConfigPath, getConfigPaths, initConfigFile, loadConfig, toCaptureOptions)
import Control.Exception (IOException, catch)
import Data.Aeson qualified as Aeson
import Data.ByteString.Char8 qualified as BS
import Data.ByteString.Lazy.Char8 qualified as BL
import Data.Maybe (fromMaybe)
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
import Types (CollectEvent, defaultCollectOptions)

newtype Instructions = Instructions Dispatch
    deriving (Show)

instance HasParser Instructions where
    settingsParser = Instructions <$> settingsParser

data Dispatch
    = DispatchCollect CollectSettings
    | DispatchConfig ConfigCommand
    deriving (Show)

instance HasParser Dispatch where
    settingsParser =
        commands
            [ command "collect" "Collect logs from a file and output JSONL events" $
                DispatchCollect <$> settingsParser
            , command "config" "Manage configuration" $
                DispatchConfig <$> settingsParser
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

runCollect :: FilePath -> IO ()
runCollect logfilePath = do
    config <- loadConfig
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
    config <- loadConfig
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
            else do
                cwd <- getCurrentDirectory
                pure $ fromMaybe cwd (initPath settings)
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
            actualPath <-
                if initUser settings
                    then getConfigPath
                    else do
                        cwd <- getCurrentDirectory
                        let basePath = fromMaybe cwd (initPath settings)
                        -- If basePath is directory, file will be .spanshot.yaml inside
                        isDir <- doesDirectoryExist basePath
                        pure $
                            if isDir
                                then basePath </> ".spanshot.yaml"
                                else basePath
            putStrLn $ "Created config file: " ++ actualPath

-- | Print a config path with existence indicator
printPathInfo :: String -> ConfigPathInfo -> IO ()
printPathInfo label info = do
    let status = if cpiExists info then "[found]" else "[not found]"
    putStrLn $ label ++ ": " ++ cpiPath info ++ " " ++ status

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
