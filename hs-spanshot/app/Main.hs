module Main where

import Collect (collectFromFileWithCleanup)
import Control.Exception (IOException, catch)
import Data.Aeson qualified as Aeson
import Data.ByteString.Lazy.Char8 qualified as BL
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

newtype Dispatch
    = DispatchCollect CollectSettings
    deriving (Show)

instance HasParser Dispatch where
    settingsParser =
        commands
            [ command "collect" "Collect logs from a file and output JSONL events" $
                DispatchCollect <$> settingsParser
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

runCollect :: FilePath -> IO ()
runCollect logfilePath =
    collectFromFileWithCleanup defaultCollectOptions logfilePath $ \events ->
        S.mapM_ printEvent events

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
