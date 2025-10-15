module Main where

import Collect (collectFromFile)
import Data.Aeson qualified as Aeson
import Data.ByteString.Lazy.Char8 qualified as BL
import OptEnvConf
    ( command,
      commands,
      help,
      metavar,
      name,
      reader,
      runSettingsParser,
      setting,
      str,
      withoutConfig,
      HasParser(..) )
import Paths_hs_spanshot (version)
import Streaming.Prelude qualified as S
import System.IO (hFlush, stdout)
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
      S.mapM_ printEvent $ collectFromFile defaultCollectOptions logfilePath

printEvent :: CollectEvent -> IO ()
printEvent event = do
  BL.putStrLn $ Aeson.encode event
  hFlush stdout
