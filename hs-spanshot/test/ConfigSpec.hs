module ConfigSpec (configTests) where

import Data.ByteString.Char8 qualified as BS
import Data.Yaml qualified as Yaml
import Data.Either (isLeft)
import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy)

import Config (CaptureConfig (..), Config (..), defaultConfig, fromCaptureOptions, toCaptureOptions)
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
