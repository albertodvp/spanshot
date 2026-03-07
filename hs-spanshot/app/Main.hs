module Main where

import Config (ConfigPathInfo (..), ConfigPaths (..), ConfigWarning (..), InitConfigError (..), capture, getConfigPath, getConfigPaths, getProjectConfigPath, initConfigFile, loadConfig, toCaptureOptions)
import Data.Aeson qualified as Aeson
import Data.ByteString.Char8 qualified as BS
import Data.ByteString.Lazy.Char8 qualified as BL

import Control.Monad (when)
import Data.Text (Text)
import Data.Text qualified as T
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
import Storage qualified
import System.Directory (doesDirectoryExist, getCurrentDirectory)
import System.Exit (exitFailure, exitWith)
import System.FilePath ((</>))
import System.IO (hFlush, hPutStrLn, stderr, stdout)
import Types (CollectEvent (..), SpanShot (..))
import Wrap qualified

newtype Instructions = Instructions Dispatch
    deriving (Show)

instance HasParser Instructions where
    settingsParser = Instructions <$> settingsParser

data Dispatch
    = DispatchConfig ConfigCommand
    | DispatchExec WrapSettings
    | DispatchCaptures CapturesCommand
    deriving (Show)

-- | Subcommands for viewing captures
data CapturesCommand
    = CapturesList
    | CapturesShow ShowSettings
    deriving (Show)

instance HasParser CapturesCommand where
    settingsParser =
        commands
            [ command "list" "List recent captures" $ pure CapturesList
            , command "show" "Show a specific capture by index" $ CapturesShow <$> settingsParser
            ]

instance HasParser Dispatch where
    settingsParser =
        commands
            [ command "exec" "Run a command with SpanShot monitoring, preserving exit code" $
                DispatchExec <$> settingsParser
            , command "captures" "View stored captures" $
                DispatchCaptures <$> settingsParser
            , command "config" "Manage configuration" $
                DispatchConfig <$> settingsParser
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

-- | Settings for the 'exec' command
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

-- | Settings for the 'show' command (User Story 3)
data ShowSettings = ShowSettings
    { showIndex :: Int
    -- ^ 1-based index of capture to show
    , showJson :: Bool
    -- ^ Output as JSON instead of formatted text
    }
    deriving (Show)

instance HasParser ShowSettings where
    settingsParser =
        ShowSettings
            <$> withoutConfig
                ( setting
                    [ help "Index of the capture to show (1 = most recent)"
                    , reader auto
                    , argument
                    , metavar "INDEX"
                    ]
                )
            <*> withoutConfig
                ( setting
                    [ help "Output as JSON"
                    , switch True
                    , long "json"
                    , value False
                    ]
                )

main :: IO ()
main = do
    Instructions dispatch <-
        runSettingsParser
            version
            "SpanShot - Log collector and analyzer"
    case dispatch of
        DispatchExec settings ->
            runExecCommand settings
        DispatchCaptures cmd ->
            runCapturesCommand cmd
        DispatchConfig cmd ->
            runConfig cmd

-- | Run the exec command: run a command with SpanShot monitoring
runExecCommand :: WrapSettings -> IO ()
runExecCommand settings = do
    case wrapCommand settings of
        [] -> do
            hPutStrLn stderr "Error: No command specified. Usage: spanshot exec COMMAND [ARGS...]"
            exitFailure
        (cmd : args) -> do
            result <- Wrap.runWrap cmd args
            exitWith (Wrap.wrapExitCode result)

-- | Run the captures subcommand
runCapturesCommand :: CapturesCommand -> IO ()
runCapturesCommand CapturesList = runListCaptures
runCapturesCommand (CapturesShow settings) = runShowCapture settings

-- | Run the captures list command: show recent captures
runListCaptures :: IO ()
runListCaptures = do
    infos <- Storage.listCapturesWithInfo
    if null infos
        then putStrLn "No captures found."
        else do
            putStrLn $ "Found " ++ show (length infos) ++ " capture(s):\n"
            mapM_ printCaptureInfo (zip [1 :: Int ..] infos)
  where
    printCaptureInfo (idx, (captureId, info)) = do
        let timestamp = show (Storage.ciCapturedAt info)
        let truncatedLine = take 60 (show (Storage.ciTriggerLine info))
        putStrLn $ "  [" ++ show idx ++ "] " ++ timestamp
        putStrLn $ "      " ++ truncatedLine
        putStrLn $ "      ID: " ++ show captureId
        putStrLn ""

-- | Run the captures show command: display a specific capture
runShowCapture :: ShowSettings -> IO ()
runShowCapture settings = do
    result <- Storage.getCaptureByIndex (showIndex settings)
    case result of
        Left err -> do
            hPutStrLn stderr $ "Error: " ++ err
            exitFailure
        Right (captureId, shot) ->
            if showJson settings
                then printJsonLn shot
                else printCaptureDetails captureId shot

-- | Print capture details in human-readable format
printCaptureDetails :: Text -> SpanShot -> IO ()
printCaptureDetails cid shot = do
    putStrLn $ "Capture ID: " ++ T.unpack cid
    putStrLn $ "Captured at: " ++ show (capturedAtUtc shot)
    putStrLn $ "Detected by: " ++ show (detectedBy shot)
    putStrLn ""
    putStrLn "=== Pre-window events ==="
    mapM_ printEventLine (preWindow shot)
    putStrLn ""
    putStrLn "=== Error event ==="
    printEventLine (errorEvent shot)
    putStrLn ""
    putStrLn "=== Post-window events ==="
    mapM_ printEventLine (postWindow shot)
  where
    printEventLine event = do
        let ts = show (readAtUtc event)
        let content = T.unpack (line event)
        putStrLn $ ts ++ " | " ++ content

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

-- | Print any JSON-serializable value as JSONL (one line, flushed to stdout)
printJsonLn :: (Aeson.ToJSON a) => a -> IO ()
printJsonLn x = do
    BL.putStrLn $ Aeson.encode x
    hFlush stdout
