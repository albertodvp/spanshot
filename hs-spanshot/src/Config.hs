{-# LANGUAGE DeriveGeneric #-}

module Config (
    Config (..),
    CaptureConfig (..),
    defaultConfig,
    loadConfig,
    getConfigPath,
    toCaptureOptions,
    fromCaptureOptions,
) where

import Data.Aeson (FromJSON, Options (fieldLabelModifier), ToJSON, camelTo2, defaultOptions, genericParseJSON, genericToJSON)
import Data.Time (NominalDiffTime)
import Data.Yaml qualified as Yaml
import GHC.Generics (Generic)
import System.Directory (XdgDirectory (XdgConfig), doesFileExist, getXdgDirectory)
import System.FilePath ((</>))

import Types (CaptureOptions (..), DetectionRule, defaultCaptureOptions, mkCaptureOptions)

-- | Top-level config wrapping all sections
data Config = Config
    { capture :: !CaptureConfig
    }
    deriving (Show, Eq, Generic)

instance ToJSON Config where
    toJSON = genericToJSON defaultOptions{fieldLabelModifier = camelTo2 '_'}

instance FromJSON Config where
    parseJSON = genericParseJSON defaultOptions{fieldLabelModifier = camelTo2 '_'}

-- | Capture phase configuration
-- Mirrors CaptureOptions but with proper YAML serialization
data CaptureConfig = CaptureConfig
    { ccPreWindowDuration :: !NominalDiffTime
    , ccPostWindowDuration :: !NominalDiffTime
    , ccMinContextEvents :: !Int
    , ccDetectionRules :: ![DetectionRule]
    }
    deriving (Show, Eq, Generic)

-- Use field prefix stripping + snake_case
captureConfigOptions :: Options
captureConfigOptions =
    defaultOptions
        { fieldLabelModifier = camelTo2 '_' . drop 2 -- drop "cc" prefix
        }

instance ToJSON CaptureConfig where
    toJSON = genericToJSON captureConfigOptions

instance FromJSON CaptureConfig where
    parseJSON = genericParseJSON captureConfigOptions

-- | Default configuration (matches defaultCaptureOptions)
defaultConfig :: Config
defaultConfig =
    Config
        { capture = fromCaptureOptions defaultCaptureOptions
        }

-- | Convert CaptureConfig to CaptureOptions (with validation)
toCaptureOptions :: CaptureConfig -> Either String CaptureOptions
toCaptureOptions cc =
    mkCaptureOptions
        (ccPreWindowDuration cc)
        (ccPostWindowDuration cc)
        (ccMinContextEvents cc)
        (ccDetectionRules cc)

-- | Convert CaptureOptions to CaptureConfig
fromCaptureOptions :: CaptureOptions -> CaptureConfig
fromCaptureOptions opts =
    CaptureConfig
        { ccPreWindowDuration = preWindowDuration opts
        , ccPostWindowDuration = postWindowDuration opts
        , ccMinContextEvents = minContextEvents opts
        , ccDetectionRules = detectionRules opts
        }

-- | Get the config file path (~/.config/spanshot/config.yaml)
getConfigPath :: IO FilePath
getConfigPath = do
    configDir <- getXdgDirectory XdgConfig "spanshot"
    pure $ configDir </> "config.yaml"

-- | Load config from file, falling back to defaults if not found
loadConfig :: IO Config
loadConfig = do
    path <- getConfigPath
    exists <- doesFileExist path
    if exists
        then do
            result <- Yaml.decodeFileEither path
            case result of
                Left err -> do
                    -- Log warning and use defaults
                    putStrLn $ "Warning: Failed to parse config: " ++ show err
                    pure defaultConfig
                Right config -> pure config
        else pure defaultConfig
