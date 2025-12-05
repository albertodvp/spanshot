{-# LANGUAGE DeriveGeneric #-}

module Config (
    -- * Configuration Types
    Config (..),
    CaptureConfig (..),
    PartialConfig (..),
    PartialCaptureConfig (..),

    -- * Config Path Types
    ConfigPathInfo (..),
    ConfigPaths (..),

    -- * Error Types
    InitConfigError (..),

    -- * Warning Types
    ConfigWarning (..),

    -- * Constants
    projectConfigFileName,
    gitDirName,
    userConfigFileName,

    -- * Default Configuration
    defaultConfig,

    -- * Config Loading
    loadConfig,
    loadConfigFrom,

    -- * Path Operations
    getConfigPath,
    getProjectConfigPath,
    getConfigPaths,
    findProjectRoot,

    -- * Config Conversion
    toCaptureOptions,
    fromCaptureOptions,

    -- * Config Merging
    mergeConfig,

    -- * Config Initialization
    initConfigFile,
) where

import Control.Exception (IOException, try)
import Data.Aeson (FromJSON, Options (fieldLabelModifier), ToJSON, camelTo2, defaultOptions, genericParseJSON, genericToJSON)
import Data.ByteString qualified as BS
import Data.Maybe (fromMaybe)
import Data.Time (NominalDiffTime)
import Data.Yaml qualified as Yaml
import GHC.Generics (Generic)
import System.Directory (XdgDirectory (XdgConfig), canonicalizePath, createDirectoryIfMissing, doesDirectoryExist, doesFileExist, getCurrentDirectory, getXdgDirectory)
import System.FilePath (takeDirectory, (</>))

import Types (CaptureOptions (..), DetectionRule, defaultCaptureOptions, mkCaptureOptions)

-- * Constants

-- | Name of the project-level config file
projectConfigFileName :: FilePath
projectConfigFileName = ".spanshot.yaml"

-- | Name of the git directory (used for project root detection)
gitDirName :: FilePath
gitDirName = ".git"

-- | Name of the user config file
userConfigFileName :: FilePath
userConfigFileName = "config.yaml"

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
        { ccPreWindowDuration = fromMaybe (ccPreWindowDuration base) (pccPreWindowDuration partial)
        , ccPostWindowDuration = fromMaybe (ccPostWindowDuration base) (pccPostWindowDuration partial)
        , ccMinContextEvents = fromMaybe (ccMinContextEvents base) (pccMinContextEvents partial)
        , ccDetectionRules = fromMaybe (ccDetectionRules base) (pccDetectionRules partial)
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
    pure $ configDir </> userConfigFileName

{- | Find the project root by looking for a .git directory or file.

Traverses up the directory tree from the given starting directory until
a .git marker is found or the filesystem root is reached.

== Git Detection

The function checks for .git as either:

* A __directory__ (normal git repository)
* A __file__ (git worktree - contains "gitdir: /path/to/main/repo/.git/worktrees/name")

This ensures proper detection in both standard git repositories and
worktree checkouts.

== Path Handling

The input path is canonicalized (resolved to an absolute path with symlinks
resolved) to ensure consistent behavior regardless of:

* Relative vs absolute input paths
* Symlinks in the path
* Current working directory changes

== Algorithm

1. Canonicalize the input path to an absolute path
2. Check if @startDir \/ .git@ exists (as file or directory)
3. If found, return @Just startDir@
4. If not found, recurse to parent directory
5. If parent == current (filesystem root), return @Nothing@

== Examples

>>> findProjectRoot "/home/user/project/src/module"
Just "/home/user/project"  -- if /home/user/project/.git exists

>>> findProjectRoot "/tmp/not-a-repo"
Nothing  -- if no .git found up to root

>>> findProjectRoot "."
Just "/home/user/project"  -- relative paths are resolved
-}
findProjectRoot :: FilePath -> IO (Maybe FilePath)
findProjectRoot dir = do
    -- Canonicalize to absolute path, resolving symlinks
    absDir <- canonicalizePath dir
    go absDir
  where
    go d = do
        let gitPath = d </> gitDirName
        -- Check for .git as directory (normal repo) or file (worktree)
        isGitDir <- doesDirectoryExist gitPath
        isGitFile <- doesFileExist gitPath
        if isGitDir || isGitFile
            then pure (Just d)
            else do
                let parent = takeDirectory d
                -- Stop if we've reached the filesystem root
                if parent == d
                    then pure Nothing
                    else go parent

{- | Get the project config file path (.spanshot.yaml in project root).

Returns @Nothing@ if no project root is found (i.e., not inside a git repository).
The project config path is @\<project-root\> \/ .spanshot.yaml@.
-}
getProjectConfigPath :: FilePath -> IO (Maybe FilePath)
getProjectConfigPath startDir = do
    projectRoot <- findProjectRoot startDir
    pure $ fmap (\root -> root </> projectConfigFileName) projectRoot

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

{- | Load config from current directory with hierarchical loading.
Loads user config first, then merges project config on top.
Returns the merged config along with any warnings encountered during loading.
-}
loadConfig :: IO (Config, [ConfigWarning])
loadConfig = do
    cwd <- getCurrentDirectory
    loadConfigFrom cwd

{- | Load config from a specific directory with hierarchical loading.
Order of precedence (lowest to highest):
1. Default config
2. User config (~/.config/spanshot/config.yaml)
3. Project config (.spanshot.yaml in project root)

Returns the merged config along with any warnings encountered during loading.
-}
loadConfigFrom :: FilePath -> IO (Config, [ConfigWarning])
loadConfigFrom startDir = do
    -- Load user config (full Config)
    (userConfig, userWarnings) <- loadUserConfig

    -- Load project config (partial, for merging)
    (projectPartial, projectWarnings) <- loadProjectConfig startDir

    -- Merge: defaults -> user -> project
    -- User config replaces defaults entirely (it's a full Config)
    -- Project config merges field-by-field on top
    let mergedConfig = mergeConfig userConfig projectPartial
        allWarnings = userWarnings ++ projectWarnings

    pure (mergedConfig, allWarnings)

-- | Load user config from XDG path, returning any parse warnings
loadUserConfig :: IO (Config, [ConfigWarning])
loadUserConfig = do
    path <- getConfigPath
    exists <- doesFileExist path
    if exists
        then do
            result <- Yaml.decodeFileEither path
            case result of
                Left err ->
                    let warning = ConfigParseWarning path (show err)
                     in pure (defaultConfig, [warning])
                Right config -> pure (config, [])
        else pure (defaultConfig, [])

-- | Load project config as PartialConfig for merging, returning any parse warnings
loadProjectConfig :: FilePath -> IO (PartialConfig, [ConfigWarning])
loadProjectConfig startDir = do
    projectPathMaybe <- getProjectConfigPath startDir
    case projectPathMaybe of
        Nothing -> pure (emptyPartialConfig, [])
        Just path -> do
            exists <- doesFileExist path
            if exists
                then do
                    result <- Yaml.decodeFileEither path
                    case result of
                        Left err ->
                            let warning = ConfigParseWarning path (show err)
                             in pure (emptyPartialConfig, [warning])
                        Right partial -> pure (partial, [])
                else pure (emptyPartialConfig, [])

-- | Empty partial config (no overrides)
emptyPartialConfig :: PartialConfig
emptyPartialConfig = PartialConfig{pcCapture = Nothing}

-- | Errors that can occur when initializing a config file
data InitConfigError
    = ConfigFileExists FilePath
    | InitIOError FilePath String
    deriving (Show, Eq)

-- | Warnings that can occur during config loading
data ConfigWarning
    = -- | A config file failed to parse (path, error message)
      ConfigParseWarning FilePath String
    deriving (Show, Eq)

{- | Initialize a config file at the specified path

If the path is a directory, creates .spanshot.yaml inside it.
If the path is a file path, creates the file at that path.
Creates parent directories if they don't exist.

Returns Left error if:
- File already exists and force is False
- IO error occurs

Returns Right () on success.
-}
initConfigFile :: FilePath -> Bool -> IO (Either InitConfigError ())
initConfigFile path force = do
    -- Check if path is an existing directory
    isDir <- doesDirectoryExist path
    let targetPath =
            if isDir
                then path </> ".spanshot.yaml"
                else path

    -- Check if file already exists
    exists <- doesFileExist targetPath
    if exists && not force
        then pure $ Left (ConfigFileExists targetPath)
        else do
            -- Create parent directories and write file, catching IO exceptions
            let parentDir = takeDirectory targetPath
            result <- try $ do
                createDirectoryIfMissing True parentDir
                let configYaml = Yaml.encode defaultConfig
                BS.writeFile targetPath configYaml
            case result of
                Left (e :: IOException) -> pure $ Left (InitIOError targetPath (show e))
                Right () -> pure $ Right ()
