module Main where

import Capture (captureFromStream)
import Collect (collectFromFileWithCleanup)
import Control.Exception (IOException, catch)
import Data.Aeson qualified as Aeson
import Data.ByteString.Lazy.Char8 qualified as BL
import OptEnvConf (
    HasParser (settingsParser),
    auto,
    command,
    commands,
    help,
    metavar,
    name,
    reader,
    runSettingsParser,
    setting,
    str,
    value,
    withoutConfig,
 )
import Paths_hs_spanshot (version)
import Streaming.Prelude qualified as S
import System.Exit (exitFailure)
import System.IO (hFlush, hPutStrLn, stderr, stdout)
import System.IO.Error (isDoesNotExistError, isPermissionError)
import Types (
    CaptureOptions,
    CollectEvent,
    DetectionRule (RegexRule),
    SpanShot,
    defaultCollectOptions,
    mkCaptureOptions,
 )

newtype Instructions = Instructions Dispatch
    deriving (Show)

instance HasParser Instructions where
    settingsParser = Instructions <$> settingsParser

data Dispatch
    = DispatchCollect CollectSettings
    | DispatchCapture CaptureSettings
    | DispatchRun RunSettings
    deriving (Show)

instance HasParser Dispatch where
    settingsParser =
        commands
            [ command "collect" "Collect logs from a file and output JSONL events" $
                DispatchCollect <$> settingsParser
            , command "capture" "Capture error spans from a log file" $
                DispatchCapture <$> settingsParser
            , command "run" "Collect and capture errors from a log file (full pipeline)" $
                DispatchRun <$> settingsParser
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

-- | Settings for capture and run commands
data CaptureSettings = CaptureSettings
    { captureLogfile :: FilePath
    , captureRegexPattern :: String
    , capturePreWindow :: Int -- seconds (integer for simplicity)
    , capturePostWindow :: Int -- seconds (integer for simplicity)
    , captureMinContext :: Int
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
            <*> withoutConfig
                ( setting
                    [ help "Regex pattern to detect errors"
                    , reader str
                    , name "regex-pattern"
                    , metavar "PATTERN"
                    ]
                )
            <*> withoutConfig
                ( setting
                    [ help "Pre-window duration in seconds"
                    , reader auto
                    , name "pre-window"
                    , metavar "SECONDS"
                    , value 5
                    ]
                )
            <*> withoutConfig
                ( setting
                    [ help "Post-window duration in seconds"
                    , reader auto
                    , name "post-window"
                    , metavar "SECONDS"
                    , value 5
                    ]
                )
            <*> withoutConfig
                ( setting
                    [ help "Minimum context events to keep"
                    , reader auto
                    , name "min-context"
                    , metavar "COUNT"
                    , value 10
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
        DispatchCapture settings ->
            runCapture settings `catch` handleIOError (captureLogfile settings)
        DispatchRun settings ->
            runFullPipeline settings `catch` handleIOError (captureLogfile settings)

runCollect :: FilePath -> IO ()
runCollect logfilePath =
    collectFromFileWithCleanup defaultCollectOptions logfilePath $ \events ->
        S.mapM_ printEvent events

runCapture :: CaptureSettings -> IO ()
runCapture settings = do
    case mkCaptureOptionsFromSettings settings of
        Left err -> do
            hPutStrLn stderr $ "Error: " ++ err
            exitFailure
        Right opts ->
            collectFromFileWithCleanup defaultCollectOptions (captureLogfile settings) $ \events ->
                S.mapM_ printSpanShot $ captureFromStream opts events

-- | Full pipeline (collect + capture combined) - same implementation as capture
runFullPipeline :: RunSettings -> IO ()
runFullPipeline = runCapture

-- | Convert CLI settings to CaptureOptions
mkCaptureOptionsFromSettings :: CaptureSettings -> Either String CaptureOptions
mkCaptureOptionsFromSettings settings =
    mkCaptureOptions
        (fromIntegral $ capturePreWindow settings)
        (fromIntegral $ capturePostWindow settings)
        (captureMinContext settings)
        [RegexRule (captureRegexPattern settings)]

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
