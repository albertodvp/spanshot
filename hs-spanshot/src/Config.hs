{-# LANGUAGE DeriveGeneric #-}

module Config (
    -- * Configuration Types
    CaptureConfig (..),
    Config (..),

    -- * Config Path Types
    ConfigPathInfo (..),
    ConfigPaths (..),

    -- * Error Types
    InitConfigError (..),

    -- * Constants
    projectConfigFileName,
    gitDirName,
    userConfigFileName,

    -- * Default Configuration
    defaultCaptureConfig,
    defaultConfig,

    -- * Path Operations
    getConfigPath,
    getProjectConfigPath,
    getConfigPaths,
    findProjectRoot,
    getConfigFilePaths,

    -- * Config Conversion
    toCaptureOptions,
    fromCaptureOptions,

    -- * Config Initialization
    initConfigFile,
) where

import Autodocodec (HasCodec (..), object, optionalFieldWithDefault, (.=))
import Control.Exception (IOException, try)
import Data.Aeson (FromJSON, Options (fieldLabelModifier), ToJSON, camelTo2, defaultOptions, genericParseJSON, genericToJSON)
import Data.ByteString qualified as BS
import Data.Time (NominalDiffTime)
import Data.Yaml qualified as Yaml
import GHC.Generics (Generic)
import Path (Abs, File, Path, parseAbsFile)
import Path.IO (doesFileExist)
import System.Directory (XdgDirectory (XdgConfig), canonicalizePath, createDirectoryIfMissing, getCurrentDirectory, getXdgDirectory)
import System.Directory qualified as Dir
import System.FilePath (takeDirectory, (</>))

import Types (CaptureOptions (..), DetectionRule (..), compileDetectionRules, defaultCaptureOptions)

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

{- | Capture phase configuration
Mirrors CaptureOptions but with proper YAML serialization
-}
data CaptureConfig = CaptureConfig
    { ccPreWindowDuration :: !NominalDiffTime
    , ccPostWindowDuration :: !NominalDiffTime
    , ccMinContextEvents :: !Int
    , ccDetectionRules :: ![DetectionRule]
    , ccInactivityTimeout :: !NominalDiffTime
    }
    deriving (Show, Eq, Generic)

-- | HasCodec instance for opt-env-conf integration
instance HasCodec CaptureConfig where
    codec =
        object "CaptureConfig" $
            CaptureConfig
                <$> optionalFieldWithDefault "pre_window_duration" defaultPreWindow "Pre-window duration in seconds"
                    .= ccPreWindowDuration
                <*> optionalFieldWithDefault "post_window_duration" defaultPostWindow "Post-window duration in seconds"
                    .= ccPostWindowDuration
                <*> optionalFieldWithDefault "min_context_events" defaultMinContext "Minimum number of context events"
                    .= ccMinContextEvents
                <*> optionalFieldWithDefault "detection_rules" defaultRules "Detection rules for error matching"
                    .= ccDetectionRules
                <*> optionalFieldWithDefault "inactivity_timeout" defaultTimeout "Inactivity timeout in seconds"
                    .= ccInactivityTimeout
      where
        defaultPreWindow = preWindowDuration defaultCaptureOptions
        defaultPostWindow = postWindowDuration defaultCaptureOptions
        defaultMinContext = minContextEvents defaultCaptureOptions
        defaultRules = detectionRules defaultCaptureOptions
        defaultTimeout = inactivityTimeout defaultCaptureOptions

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

-- | Default capture configuration (matches defaultCaptureOptions)
defaultCaptureConfig :: CaptureConfig
defaultCaptureConfig = fromCaptureOptions defaultCaptureOptions

-- | Top-level config structure (for YAML file format)
newtype Config = Config
    { capture :: CaptureConfig
    }
    deriving (Show, Eq, Generic)

instance ToJSON Config where
    toJSON = genericToJSON defaultOptions{fieldLabelModifier = camelTo2 '_'}

instance FromJSON Config where
    parseJSON = genericParseJSON defaultOptions{fieldLabelModifier = camelTo2 '_'}

-- | Default top-level config
defaultConfig :: Config
defaultConfig = Config defaultCaptureConfig

{- | Convert CaptureConfig to CaptureOptions (with validation and regex compilation).

Validates all fields and pre-compiles regex patterns for efficient matching.
Returns Left with an error message if validation fails or any regex is invalid.

Pre-compiling regexes is important for performance: without it, each event would
require re-compiling all regex patterns, which is expensive.
-}
toCaptureOptions :: CaptureConfig -> Either String CaptureOptions
toCaptureOptions cc
    | ccPreWindowDuration cc < 0 =
        Left "preWindowDuration must be non-negative (>= 0 seconds)"
    | ccPostWindowDuration cc < 0 =
        Left "postWindowDuration must be non-negative (>= 0 seconds)"
    | ccPreWindowDuration cc == 0 && ccPostWindowDuration cc == 0 =
        Left "At least one of preWindowDuration or postWindowDuration must be positive (> 0)"
    | ccMinContextEvents cc < 1 =
        Left "minContextEvents must be at least 1"
    | null (ccDetectionRules cc) =
        Left "detectionRules cannot be empty"
    | ccInactivityTimeout cc <= 0 =
        Left "inactivityTimeout must be positive (> 0 seconds)"
    | ccPostWindowDuration cc > 0 && ccInactivityTimeout cc < ccPostWindowDuration cc =
        Left "inactivityTimeout must be at least postWindowDuration"
    | otherwise =
        case compileDetectionRules (ccDetectionRules cc) of
            Left err -> Left err
            Right compiled ->
                Right $
                    CaptureOptions
                        { preWindowDuration = ccPreWindowDuration cc
                        , postWindowDuration = ccPostWindowDuration cc
                        , minContextEvents = ccMinContextEvents cc
                        , detectionRules = ccDetectionRules cc
                        , compiledRules = compiled
                        , inactivityTimeout = ccInactivityTimeout cc
                        }

-- | Convert CaptureOptions to CaptureConfig
fromCaptureOptions :: CaptureOptions -> CaptureConfig
fromCaptureOptions opts =
    CaptureConfig
        { ccPreWindowDuration = preWindowDuration opts
        , ccPostWindowDuration = postWindowDuration opts
        , ccMinContextEvents = minContextEvents opts
        , ccDetectionRules = detectionRules opts
        , ccInactivityTimeout = inactivityTimeout opts
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
        isGitDir <- Dir.doesDirectoryExist gitPath
        isGitFile <- Dir.doesFileExist gitPath
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
    userExists <- Dir.doesFileExist userPath
    let userInfo = ConfigPathInfo{cpiPath = userPath, cpiExists = userExists}

    -- Project config
    projectPathMaybe <- getProjectConfigPath startDir
    projectInfo <- case projectPathMaybe of
        Nothing -> pure Nothing
        Just path -> do
            exists <- Dir.doesFileExist path
            pure $ Just ConfigPathInfo{cpiPath = path, cpiExists = exists}

    pure ConfigPaths{cpiUser = userInfo, cpiProject = projectInfo}

{- | Get config file paths for opt-env-conf's withCombinedYamlConfigs.

Returns a list of existing config file paths as Path Abs File,
in order of precedence (user config first, then project config).
Only includes files that actually exist.
-}
getConfigFilePaths :: IO [Path Abs File]
getConfigFilePaths = do
    cwd <- getCurrentDirectory
    paths <- getConfigPaths cwd

    -- Collect paths that exist and can be parsed
    userPaths <- case cpiExists (cpiUser paths) of
        True -> case parseAbsFile (cpiPath (cpiUser paths)) of
            Just p -> do
                exists <- doesFileExist p
                pure [p | exists]
            Nothing -> pure []
        False -> pure []

    projectPaths <- case cpiProject paths of
        Just info | cpiExists info -> case parseAbsFile (cpiPath info) of
            Just p -> do
                exists <- doesFileExist p
                pure [p | exists]
            Nothing -> pure []
        _ -> pure []

    -- User config first (lower precedence), then project config (higher precedence)
    pure $ userPaths ++ projectPaths

-- | Errors that can occur when initializing a config file
data InitConfigError
    = ConfigFileExists FilePath
    | InitIOError FilePath String
    deriving (Show, Eq)

{- | Initialize a config file at the specified path

If the path is a directory, creates .spanshot.yaml inside it.
If the path is a file path, creates the file at that path.
Creates parent directories if they don't exist.

Returns Left error if:
- File already exists and force is False
- IO error occurs

Returns Right () on success.

== Race Condition Note

When @force@ is False, this function checks for file existence before writing.
There is a small TOCTOU (Time-of-Check-Time-of-Use) race window where another
process could create the file between the check and write. In practice:

- This is acceptable for CLI usage where concurrent config creation is rare
- The worst case is overwriting a file created in the race window
- For critical applications, platform-specific atomic APIs should be used

The implementation prioritizes clear error messages and cross-platform
compatibility over perfect atomicity.
-}
initConfigFile :: FilePath -> Bool -> IO (Either InitConfigError ())
initConfigFile path force = do
    -- Check if path is an existing directory
    isDir <- Dir.doesDirectoryExist path
    let targetPath =
            if isDir
                then path </> ".spanshot.yaml"
                else path

    let parentDir = takeDirectory targetPath
    -- Create a top-level config structure for YAML output
    let configYaml = Yaml.encode defaultConfig

    -- Check for existing file when not forcing
    existsBeforeWrite <- Dir.doesFileExist targetPath
    if existsBeforeWrite && not force
        then pure $ Left (ConfigFileExists targetPath)
        else do
            result <- try $ do
                createDirectoryIfMissing True parentDir
                BS.writeFile targetPath configYaml
            case result of
                Left (e :: IOException) -> pure $ Left (InitIOError targetPath (show e))
                Right () -> pure $ Right ()
