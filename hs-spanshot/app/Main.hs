module Main where

import Client (ClientError (..), sendCommand)
import Collect (collectFromFileWithCleanup)
import Config (getConfigPath, loadConfig)
import Control.Exception (IOException, catch)
import Daemon.Core qualified as Daemon
import Daemon.Protocol (Request (..), Response (..), ResponseResult (..))
import Daemon.Types (DaemonStats (..))
import Data.Aeson qualified as Aeson
import Data.ByteString.Char8 qualified as BS
import Data.ByteString.Lazy.Char8 qualified as BL
import Data.Text qualified as T
import Data.Yaml qualified as Yaml
import OptEnvConf (
    HasParser (settingsParser),
    command,
    commands,
    help,
    metavar,
    name,
    reader,
    runSettingsParser,
    setting,
    str,
    withoutConfig,
 )
import Paths_hs_spanshot (version)
import Streaming.Prelude qualified as S
import System.Exit (exitFailure)
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
    | DispatchDaemon DaemonCommand
    deriving (Show)

instance HasParser Dispatch where
    settingsParser =
        commands
            [ command "collect" "Collect logs from a file and output JSONL events" $
                DispatchCollect <$> settingsParser
            , command "config" "Manage configuration" $
                DispatchConfig <$> settingsParser
            , command "daemon" "Manage the background daemon" $
                DispatchDaemon <$> settingsParser
            ]

data ConfigCommand
    = ConfigShow
    | ConfigPath
    deriving (Show)

instance HasParser ConfigCommand where
    settingsParser =
        commands
            [ command "show" "Show current configuration" $ pure ConfigShow
            , command "path" "Show configuration file path" $ pure ConfigPath
            ]

data DaemonCommand
    = DaemonStart
    | DaemonStop
    | DaemonStatus
    | DaemonForeground
    deriving (Show)

instance HasParser DaemonCommand where
    settingsParser =
        commands
            [ command "start" "Start the daemon in the background" $ pure DaemonStart
            , command "stop" "Stop the running daemon" $ pure DaemonStop
            , command "status" "Check daemon status" $ pure DaemonStatus
            , command "foreground" "Run daemon in foreground (for debugging)" $ pure DaemonForeground
            ]

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
        DispatchDaemon cmd ->
            runDaemon cmd

runCollect :: FilePath -> IO ()
runCollect logfilePath =
    collectFromFileWithCleanup defaultCollectOptions logfilePath $ \events ->
        S.mapM_ printEvent events

runConfig :: ConfigCommand -> IO ()
runConfig ConfigShow = do
    config <- loadConfig
    BS.putStrLn $ Yaml.encode config
runConfig ConfigPath = do
    path <- getConfigPath
    putStrLn path

runDaemon :: DaemonCommand -> IO ()
runDaemon DaemonStart = Daemon.startDaemon
runDaemon DaemonForeground = Daemon.runDaemonForeground
runDaemon DaemonStop = do
    result <- sendCommand ReqShutdown
    case result of
        Left (ConnectionFailed _) -> putStrLn "Daemon not running"
        Left (ProtocolError e) -> putStrLn $ "Error: " ++ e
        Right _ -> putStrLn "Daemon stopped"
runDaemon DaemonStatus = do
    result <- sendCommand ReqStatus
    case result of
        Left (ConnectionFailed _) -> putStrLn "Daemon not running"
        Left (ProtocolError e) -> putStrLn $ "Error: " ++ e
        Right (RespOk (ResultStatus stats)) -> do
            putStrLn "Status: running"
            putStrLn $ "PID: " ++ show (statsPid stats)
            putStrLn $ "Uptime: " ++ show (statsUptime stats) ++ "s"
            putStrLn $ "Watches: " ++ show (statsWatchCount stats)
            putStrLn $ "Errors captured: " ++ show (statsErrorCount stats)
        Right (RespError e) -> putStrLn $ "Error: " ++ T.unpack e
        Right _ -> putStrLn "Unexpected response"

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
