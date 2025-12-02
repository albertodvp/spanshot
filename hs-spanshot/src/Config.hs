{-# LANGUAGE DeriveGeneric #-}

module Config (
    Config (..),
    CaptureConfig (..),
    PartialConfig (..),
    PartialCaptureConfig (..),
    ConfigPathInfo (..),
    ConfigPaths (..),
    defaultConfig,
    loadConfig,
    loadConfigFrom,
    getConfigPath,
    getProjectConfigPath,
    getConfigPaths,
    findProjectRoot,
    toCaptureOptions,
    fromCaptureOptions,
    mergeConfig,
) where

import Data.Aeson (FromJSON, Options (fieldLabelModifier), ToJSON, camelTo2, defaultOptions, genericParseJSON, genericToJSON)
import Data.Time (NominalDiffTime)
import Data.Yaml qualified as Yaml
import GHC.Generics (Generic)
import System.Directory (XdgDirectory (XdgConfig), doesDirectoryExist, doesFileExist, getCurrentDirectory, getXdgDirectory)
import System.FilePath (takeDirectory, (</>))
import System.IO (hPutStrLn, stderr)

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

{- | Capture phase configuration
Mirrors CaptureOptions but with proper YAML serialization
-}
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

{- | Partial configuration for merging (all fields optional)
Used for project-level config that overrides user config field-by-field
-}
data PartialConfig = PartialConfig
    { pcCapture :: !(Maybe PartialCaptureConfig)
    }
    deriving (Show, Eq, Generic)

instance ToJSON PartialConfig where
    toJSON = genericToJSON defaultOptions{fieldLabelModifier = camelTo2 '_' . drop 2}

instance FromJSON PartialConfig where
    parseJSON = genericParseJSON defaultOptions{fieldLabelModifier = camelTo2 '_' . drop 2}

-- | Partial capture config with all fields optional
data PartialCaptureConfig = PartialCaptureConfig
    { pccPreWindowDuration :: !(Maybe NominalDiffTime)
    , pccPostWindowDuration :: !(Maybe NominalDiffTime)
    , pccMinContextEvents :: !(Maybe Int)
    , pccDetectionRules :: !(Maybe [DetectionRule])
    }
    deriving (Show, Eq, Generic)

-- Use field prefix stripping + snake_case for partial config
partialCaptureConfigOptions :: Options
partialCaptureConfigOptions =
    defaultOptions
        { fieldLabelModifier = camelTo2 '_' . drop 3 -- drop "pcc" prefix
        }

instance ToJSON PartialCaptureConfig where
    toJSON = genericToJSON partialCaptureConfigOptions

instance FromJSON PartialCaptureConfig where
    parseJSON = genericParseJSON partialCaptureConfigOptions

{- | Merge a base config with a partial override config
Override values take precedence over base values when present
-}
mergeConfig :: Config -> PartialConfig -> Config
mergeConfig base override =
    Config
        { capture = mergeCaptureConfig (capture base) (pcCapture override)
        }

-- | Merge capture configs, with partial override taking precedence
mergeCaptureConfig :: CaptureConfig -> Maybe PartialCaptureConfig -> CaptureConfig
mergeCaptureConfig base Nothing = base
mergeCaptureConfig base (Just partial) =
    CaptureConfig
        { ccPreWindowDuration = maybe (ccPreWindowDuration base) id (pccPreWindowDuration partial)
        , ccPostWindowDuration = maybe (ccPostWindowDuration base) id (pccPostWindowDuration partial)
        , ccMinContextEvents = maybe (ccMinContextEvents base) id (pccMinContextEvents partial)
        , ccDetectionRules = maybe (ccDetectionRules base) id (pccDetectionRules partial)
        }

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

-- | Get the user config file path (~/.config/spanshot/config.yaml)
getConfigPath :: IO FilePath
getConfigPath = do
    configDir <- getXdgDirectory XdgConfig "spanshot"
    pure $ configDir </> "config.yaml"

{- | Find the project root by looking for a .git directory
Traverses up from the given directory until .git is found or root is reached
-}
findProjectRoot :: FilePath -> IO (Maybe FilePath)
findProjectRoot dir = do
    let gitDir = dir </> ".git"
    hasGit <- doesDirectoryExist gitDir
    if hasGit
        then pure (Just dir)
        else do
            let parent = takeDirectory dir
            -- Stop if we've reached the filesystem root
            if parent == dir
                then pure Nothing
                else findProjectRoot parent

{- | Get the project config file path (.spanshot.yaml in project root)
Returns Nothing if no project root is found
-}
getProjectConfigPath :: FilePath -> IO (Maybe FilePath)
getProjectConfigPath startDir = do
    projectRoot <- findProjectRoot startDir
    pure $ fmap (\root -> root </> ".spanshot.yaml") projectRoot

-- | Information about a config file path
data ConfigPathInfo = ConfigPathInfo
    { cpiPath :: !FilePath
    , cpiExists :: !Bool
    }
    deriving (Show, Eq)

-- | All config paths with existence info
data ConfigPaths = ConfigPaths
    { cpiUser :: !ConfigPathInfo
    , cpiProject :: !(Maybe ConfigPathInfo)
    }
    deriving (Show, Eq)

{- | Get all config paths with existence status
Takes the starting directory for project root detection
-}
getConfigPaths :: FilePath -> IO ConfigPaths
getConfigPaths startDir = do
    -- User config
    userPath <- getConfigPath
    userExists <- doesFileExist userPath
    let userInfo = ConfigPathInfo{cpiPath = userPath, cpiExists = userExists}

    -- Project config
    projectPathMaybe <- getProjectConfigPath startDir
    projectInfo <- case projectPathMaybe of
        Nothing -> pure Nothing
        Just path -> do
            exists <- doesFileExist path
            pure $ Just ConfigPathInfo{cpiPath = path, cpiExists = exists}

    pure ConfigPaths{cpiUser = userInfo, cpiProject = projectInfo}

{- | Load config from current directory with hierarchical loading
Loads user config first, then merges project config on top
-}
loadConfig :: IO Config
loadConfig = do
    cwd <- getCurrentDirectory
    loadConfigFrom cwd

{- | Load config from a specific directory with hierarchical loading
Order of precedence (lowest to highest):
1. Default config
2. User config (~/.config/spanshot/config.yaml)
3. Project config (.spanshot.yaml in project root)
-}
loadConfigFrom :: FilePath -> IO Config
loadConfigFrom startDir = do
    -- Start with defaults
    let baseConfig = defaultConfig

    -- Load user config (full Config)
    userConfig <- loadUserConfig

    -- Load project config (partial, for merging)
    projectPartial <- loadProjectConfig startDir

    -- Merge: defaults -> user -> project
    -- User config replaces defaults entirely (it's a full Config)
    -- Project config merges field-by-field on top
    let mergedConfig = mergeConfig userConfig projectPartial

    pure mergedConfig

-- | Load user config from XDG path
loadUserConfig :: IO Config
loadUserConfig = do
    path <- getConfigPath
    exists <- doesFileExist path
    if exists
        then do
            result <- Yaml.decodeFileEither path
            case result of
                Left err -> do
                    hPutStrLn stderr $ "Warning: Failed to parse user config file " ++ path ++ ": " ++ show err
                    hPutStrLn stderr "Using default configuration."
                    pure defaultConfig
                Right config -> pure config
        else pure defaultConfig

-- | Load project config as PartialConfig for merging
loadProjectConfig :: FilePath -> IO PartialConfig
loadProjectConfig startDir = do
    projectPathMaybe <- getProjectConfigPath startDir
    case projectPathMaybe of
        Nothing -> pure emptyPartialConfig
        Just path -> do
            exists <- doesFileExist path
            if exists
                then do
                    result <- Yaml.decodeFileEither path
                    case result of
                        Left err -> do
                            hPutStrLn stderr $ "Warning: Failed to parse project config file " ++ path ++ ": " ++ show err
                            hPutStrLn stderr "Using user/default configuration."
                            pure emptyPartialConfig
                        Right partial -> pure partial
                else pure emptyPartialConfig

-- | Empty partial config (no overrides)
emptyPartialConfig :: PartialConfig
emptyPartialConfig = PartialConfig{pcCapture = Nothing}
