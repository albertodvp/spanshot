module ConfigSpec (configTests) where

import Data.ByteString.Char8 qualified as BS
import Data.Either (isLeft)
import Data.Yaml qualified as Yaml
import System.Directory (createDirectory, createDirectoryIfMissing)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec (Spec, describe, it, shouldBe, shouldReturn, shouldSatisfy)

import Config (
    CaptureConfig (..),
    Config (..),
    ConfigPathInfo (..),
    ConfigPaths (..),
    PartialCaptureConfig (..),
    PartialConfig (..),
    defaultConfig,
    findProjectRoot,
    fromCaptureOptions,
    getConfigPaths,
    getProjectConfigPath,
    loadConfigFrom,
    mergeConfig,
    toCaptureOptions,
 )
import Types (DetectionRule (RegexRule), defaultCaptureOptions)

configTests :: Spec
configTests = do
    describe "Config YAML parsing" $ do
        it "parses a valid config file" $ do
            let yaml =
                    BS.unlines
                        [ "capture:"
                        , "  pre_window_duration: 10"
                        , "  post_window_duration: 3"
                        , "  min_context_events: 5"
                        , "  detection_rules:"
                        , "    - regex_pattern: \"ERROR\""
                        , "    - regex_pattern: \"WARN\""
                        ]
            let parsed = Yaml.decodeEither' yaml :: Either Yaml.ParseException Config
            case parsed of
                Left err -> fail $ show err
                Right config -> do
                    ccPreWindowDuration (capture config) `shouldBe` 10
                    ccPostWindowDuration (capture config) `shouldBe` 3
                    ccMinContextEvents (capture config) `shouldBe` 5
                    length (ccDetectionRules (capture config)) `shouldBe` 2

        it "round-trips defaultConfig through YAML" $ do
            let encoded = Yaml.encode defaultConfig
            let decoded = Yaml.decodeEither' encoded :: Either Yaml.ParseException Config
            case decoded of
                Left err -> fail $ "Failed to decode: " ++ show err
                Right config -> config `shouldBe` defaultConfig

        it "parses config from fixture file" $ do
            content <- BS.readFile "test/fixtures/config.yaml"
            let parsed = Yaml.decodeEither' content :: Either Yaml.ParseException Config
            case parsed of
                Left err -> fail $ show err
                Right config -> do
                    ccPreWindowDuration (capture config) `shouldBe` 10
                    ccPostWindowDuration (capture config) `shouldBe` 3
                    ccMinContextEvents (capture config) `shouldBe` 5
                    ccDetectionRules (capture config)
                        `shouldBe` [ RegexRule "ERROR"
                                   , RegexRule "WARN"
                                   , RegexRule "FATAL"
                                   ]

    describe "CaptureConfig conversion" $ do
        it "converts to CaptureOptions correctly" $ do
            let captureConfig = fromCaptureOptions defaultCaptureOptions
            toCaptureOptions captureConfig `shouldBe` Right defaultCaptureOptions

        it "fromCaptureOptions preserves all fields" $ do
            let cc = fromCaptureOptions defaultCaptureOptions
            ccPreWindowDuration cc `shouldBe` 5
            ccPostWindowDuration cc `shouldBe` 5
            ccMinContextEvents cc `shouldBe` 10
            ccDetectionRules cc `shouldBe` [RegexRule "ERROR"]

        it "toCaptureOptions validates negative durations" $ do
            let invalidConfig =
                    CaptureConfig
                        { ccPreWindowDuration = -1
                        , ccPostWindowDuration = 5
                        , ccMinContextEvents = 10
                        , ccDetectionRules = [RegexRule "ERROR"]
                        }
            toCaptureOptions invalidConfig `shouldSatisfy` isLeft

        it "toCaptureOptions validates empty rules" $ do
            let invalidConfig =
                    CaptureConfig
                        { ccPreWindowDuration = 5
                        , ccPostWindowDuration = 5
                        , ccMinContextEvents = 10
                        , ccDetectionRules = []
                        }
            toCaptureOptions invalidConfig `shouldSatisfy` isLeft

    describe "Config merging" $ do
        it "returns base config when override is empty" $ do
            let base = defaultConfig
            let override = PartialConfig{pcCapture = Nothing}
            mergeConfig base override `shouldBe` base

        it "overrides preWindowDuration only" $ do
            let base = defaultConfig
            let override =
                    PartialConfig
                        { pcCapture =
                            Just
                                PartialCaptureConfig
                                    { pccPreWindowDuration = Just 20
                                    , pccPostWindowDuration = Nothing
                                    , pccMinContextEvents = Nothing
                                    , pccDetectionRules = Nothing
                                    }
                        }
            let result = mergeConfig base override
            ccPreWindowDuration (capture result) `shouldBe` 20
            -- Other fields should remain from base
            ccPostWindowDuration (capture result) `shouldBe` ccPostWindowDuration (capture base)
            ccMinContextEvents (capture result) `shouldBe` ccMinContextEvents (capture base)
            ccDetectionRules (capture result) `shouldBe` ccDetectionRules (capture base)

        it "overrides postWindowDuration only" $ do
            let base = defaultConfig
            let override =
                    PartialConfig
                        { pcCapture =
                            Just
                                PartialCaptureConfig
                                    { pccPreWindowDuration = Nothing
                                    , pccPostWindowDuration = Just 15
                                    , pccMinContextEvents = Nothing
                                    , pccDetectionRules = Nothing
                                    }
                        }
            let result = mergeConfig base override
            ccPostWindowDuration (capture result) `shouldBe` 15
            ccPreWindowDuration (capture result) `shouldBe` ccPreWindowDuration (capture base)

        it "overrides minContextEvents only" $ do
            let base = defaultConfig
            let override =
                    PartialConfig
                        { pcCapture =
                            Just
                                PartialCaptureConfig
                                    { pccPreWindowDuration = Nothing
                                    , pccPostWindowDuration = Nothing
                                    , pccMinContextEvents = Just 50
                                    , pccDetectionRules = Nothing
                                    }
                        }
            let result = mergeConfig base override
            ccMinContextEvents (capture result) `shouldBe` 50
            ccPreWindowDuration (capture result) `shouldBe` ccPreWindowDuration (capture base)

        it "overrides detectionRules completely (replaces, not merges)" $ do
            let base = defaultConfig
            let newRules = [RegexRule "FATAL", RegexRule "CRITICAL"]
            let override =
                    PartialConfig
                        { pcCapture =
                            Just
                                PartialCaptureConfig
                                    { pccPreWindowDuration = Nothing
                                    , pccPostWindowDuration = Nothing
                                    , pccMinContextEvents = Nothing
                                    , pccDetectionRules = Just newRules
                                    }
                        }
            let result = mergeConfig base override
            ccDetectionRules (capture result) `shouldBe` newRules

        it "overrides multiple fields at once" $ do
            let base = defaultConfig
            let override =
                    PartialConfig
                        { pcCapture =
                            Just
                                PartialCaptureConfig
                                    { pccPreWindowDuration = Just 30
                                    , pccPostWindowDuration = Just 25
                                    , pccMinContextEvents = Nothing
                                    , pccDetectionRules = Just [RegexRule "WARN"]
                                    }
                        }
            let result = mergeConfig base override
            ccPreWindowDuration (capture result) `shouldBe` 30
            ccPostWindowDuration (capture result) `shouldBe` 25
            ccMinContextEvents (capture result) `shouldBe` ccMinContextEvents (capture base)
            ccDetectionRules (capture result) `shouldBe` [RegexRule "WARN"]

    describe "PartialConfig YAML parsing" $ do
        it "parses partial config with only preWindowDuration" $ do
            let yaml =
                    BS.unlines
                        [ "capture:"
                        , "  pre_window_duration: 20"
                        ]
            let parsed = Yaml.decodeEither' yaml :: Either Yaml.ParseException PartialConfig
            case parsed of
                Left err -> fail $ show err
                Right pc -> do
                    case pcCapture pc of
                        Nothing -> fail "Expected capture config"
                        Just pcc -> do
                            pccPreWindowDuration pcc `shouldBe` Just 20
                            pccPostWindowDuration pcc `shouldBe` Nothing
                            pccMinContextEvents pcc `shouldBe` Nothing
                            pccDetectionRules pcc `shouldBe` Nothing

        it "parses empty partial config" $ do
            let yaml = BS.unlines ["{}"]
            let parsed = Yaml.decodeEither' yaml :: Either Yaml.ParseException PartialConfig
            case parsed of
                Left err -> fail $ show err
                Right pc -> pcCapture pc `shouldBe` Nothing

        it "parses partial config with only detection_rules" $ do
            let yaml =
                    BS.unlines
                        [ "capture:"
                        , "  detection_rules:"
                        , "    - regex_pattern: \"FATAL\""
                        ]
            let parsed = Yaml.decodeEither' yaml :: Either Yaml.ParseException PartialConfig
            case parsed of
                Left err -> fail $ show err
                Right pc -> do
                    case pcCapture pc of
                        Nothing -> fail "Expected capture config"
                        Just pcc -> do
                            pccDetectionRules pcc `shouldBe` Just [RegexRule "FATAL"]
                            pccPreWindowDuration pcc `shouldBe` Nothing

    describe "findProjectRoot" $ do
        it "finds .git in current directory" $ do
            withSystemTempDirectory "spanshot-test" $ \tmpDir -> do
                -- Create .git directory
                createDirectory (tmpDir </> ".git")
                result <- findProjectRoot tmpDir
                result `shouldBe` Just tmpDir

        it "finds .git in parent directory" $ do
            withSystemTempDirectory "spanshot-test" $ \tmpDir -> do
                -- Create .git in root and a subdirectory
                createDirectory (tmpDir </> ".git")
                let subDir = tmpDir </> "src" </> "deep"
                createDirectoryIfMissing True subDir
                result <- findProjectRoot subDir
                result `shouldBe` Just tmpDir

        it "returns Nothing when no .git found" $ do
            withSystemTempDirectory "spanshot-test" $ \tmpDir -> do
                -- No .git directory - should eventually hit root and return Nothing
                -- Create a deep path to test traversal
                let subDir = tmpDir </> "no" </> "git" </> "here"
                createDirectoryIfMissing True subDir
                result <- findProjectRoot subDir
                result `shouldBe` Nothing

        it "stops at filesystem root" $ do
            -- Test from root itself (no .git there)
            result <- findProjectRoot "/"
            result `shouldBe` Nothing

    describe "getProjectConfigPath" $ do
        it "returns path when project root exists" $ do
            withSystemTempDirectory "spanshot-test" $ \tmpDir -> do
                createDirectory (tmpDir </> ".git")
                result <- getProjectConfigPath tmpDir
                result `shouldBe` Just (tmpDir </> ".spanshot.yaml")

        it "returns path from subdirectory" $ do
            withSystemTempDirectory "spanshot-test" $ \tmpDir -> do
                createDirectory (tmpDir </> ".git")
                let subDir = tmpDir </> "src"
                createDirectory subDir
                result <- getProjectConfigPath subDir
                result `shouldBe` Just (tmpDir </> ".spanshot.yaml")

        it "returns Nothing when no project root" $ do
            withSystemTempDirectory "spanshot-test" $ \tmpDir -> do
                -- No .git directory
                result <- getProjectConfigPath tmpDir
                result `shouldBe` Nothing

    describe "getConfigPaths" $ do
        it "returns user path and no project path when not in a project" $ do
            withSystemTempDirectory "spanshot-test" $ \tmpDir -> do
                -- No .git directory
                paths <- getConfigPaths tmpDir
                -- User path should always be present
                cpiPath (cpiUser paths) `shouldSatisfy` not . null
                cpiExists (cpiUser paths) `shouldBe` False -- temp dir won't have user config
                cpiProject paths `shouldBe` Nothing

        it "returns both paths when in a project" $ do
            withSystemTempDirectory "spanshot-test" $ \tmpDir -> do
                createDirectory (tmpDir </> ".git")
                paths <- getConfigPaths tmpDir
                -- User path should always be present
                cpiPath (cpiUser paths) `shouldSatisfy` not . null
                -- Project path should be present
                case cpiProject paths of
                    Nothing -> fail "Expected project config path"
                    Just projInfo -> do
                        cpiPath projInfo `shouldBe` (tmpDir </> ".spanshot.yaml")
                        cpiExists projInfo `shouldBe` False -- file doesn't exist yet
        it "detects existing project config file" $ do
            withSystemTempDirectory "spanshot-test" $ \tmpDir -> do
                createDirectory (tmpDir </> ".git")
                -- Create actual config file
                writeFile (tmpDir </> ".spanshot.yaml") "capture:\n  pre_window_duration: 10\n"
                paths <- getConfigPaths tmpDir
                case cpiProject paths of
                    Nothing -> fail "Expected project config path"
                    Just projInfo -> do
                        cpiPath projInfo `shouldBe` (tmpDir </> ".spanshot.yaml")
                        cpiExists projInfo `shouldBe` True

    describe "loadConfigFrom (hierarchical loading)" $ do
        it "returns default config when no config files exist" $ do
            withSystemTempDirectory "spanshot-test" $ \tmpDir -> do
                -- No .git, no config files
                config <- loadConfigFrom tmpDir
                config `shouldBe` defaultConfig

        it "loads project config and merges with defaults" $ do
            withSystemTempDirectory "spanshot-test" $ \tmpDir -> do
                createDirectory (tmpDir </> ".git")
                -- Create project config with partial override
                writeFile
                    (tmpDir </> ".spanshot.yaml")
                    "capture:\n  pre_window_duration: 30\n"
                config <- loadConfigFrom tmpDir
                -- Pre-window should be overridden
                ccPreWindowDuration (capture config) `shouldBe` 30
                -- Other fields should remain as defaults
                ccPostWindowDuration (capture config) `shouldBe` ccPostWindowDuration (capture defaultConfig)
                ccMinContextEvents (capture config) `shouldBe` ccMinContextEvents (capture defaultConfig)

        it "project config overrides all specified fields" $ do
            withSystemTempDirectory "spanshot-test" $ \tmpDir -> do
                createDirectory (tmpDir </> ".git")
                writeFile
                    (tmpDir </> ".spanshot.yaml")
                    ( unlines
                        [ "capture:"
                        , "  pre_window_duration: 20"
                        , "  post_window_duration: 15"
                        , "  min_context_events: 25"
                        , "  detection_rules:"
                        , "    - regex_pattern: \"FATAL\""
                        ]
                    )
                config <- loadConfigFrom tmpDir
                ccPreWindowDuration (capture config) `shouldBe` 20
                ccPostWindowDuration (capture config) `shouldBe` 15
                ccMinContextEvents (capture config) `shouldBe` 25
                ccDetectionRules (capture config) `shouldBe` [RegexRule "FATAL"]

        it "handles invalid project config gracefully (uses defaults)" $ do
            withSystemTempDirectory "spanshot-test" $ \tmpDir -> do
                createDirectory (tmpDir </> ".git")
                -- Create invalid YAML
                writeFile (tmpDir </> ".spanshot.yaml") "invalid: yaml: content: ["
                config <- loadConfigFrom tmpDir
                -- Should fall back to defaults
                config `shouldBe` defaultConfig
