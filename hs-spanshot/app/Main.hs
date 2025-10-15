module Main where

import Collect (collectFromFile)
import Data.Aeson qualified as Aeson
import Data.ByteString.Lazy.Char8 qualified as BL
import OptEnvConf
    ( setting,
      withoutConfig,
      str,
      runSettingsParser,
      help,
      metavar,
      name,
      reader,
      HasParser(..) )
import Paths_hs_spanshot (version)
import Streaming.Prelude qualified as S
import Types (CollectEvent, defaultCollectOptions)

newtype Settings = Settings
  { logfile :: FilePath
  }

instance HasParser Settings where
  settingsParser =
    Settings
      <$> withoutConfig
        ( setting
            [ help "Path to the logfile to tail",
              reader str,
              name "logfile",
              metavar "PATH"
            ]
        )

main :: IO ()
main = do
  Settings {logfile = logfilePath} <-
    runSettingsParser
      version
      "SpanShot - Log collector and analyzer"
  S.mapM_ printEvent $ collectFromFile defaultCollectOptions logfilePath

printEvent :: CollectEvent -> IO ()
printEvent event = BL.putStrLn $ Aeson.encode event
