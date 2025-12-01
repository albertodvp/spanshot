module Main (main) where

import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.Hspec (testSpec)

import CaptureStreamSpec (captureStreamTests)
import CaptureTypesSpec (captureTypesTests)
import CollectionSpec (collectionTests)
import ConfigSpec (configTests)
import DetectionSpec (detectionTests)
import SerializationProperties (serializationPropertyTests)
import WindowManagementSpec (windowManagementTests)

-- Daemon tests
import DaemonConfigWatcherSpec (daemonConfigWatcherTests)
import DaemonProcessSpec (daemonProcessTests)
import DaemonProtocolSpec (daemonProtocolTests)
import DaemonServerSpec (daemonServerTests)
import DaemonStateSpec (daemonStateTests)

main :: IO ()
main = do
    collectionSpec <- testSpec "Collection Tests" collectionTests
    captureTypesSpec <- testSpec "Capture Types Tests" captureTypesTests
    captureStreamSpec <- testSpec "Capture Stream Tests" captureStreamTests
    configSpec <- testSpec "Config Tests" configTests
    detectionSpec <- testSpec "Detection Tests" detectionTests
    windowManagementSpec <- testSpec "Window Management Tests" windowManagementTests

    -- Daemon specs
    daemonStateSpec <- testSpec "Daemon State Tests" daemonStateTests
    daemonProtocolSpec <- testSpec "Daemon Protocol Tests" daemonProtocolTests
    daemonProcessSpec <- testSpec "Daemon Process Tests" daemonProcessTests
    daemonServerSpec <- testSpec "Daemon Server Tests" daemonServerTests
    daemonConfigWatcherSpec <- testSpec "Daemon ConfigWatcher Tests" daemonConfigWatcherTests

    defaultMain $
        testGroup
            "SpanShot Tests"
            [ collectionSpec
            , captureTypesSpec
            , captureStreamSpec
            , configSpec
            , detectionSpec
            , windowManagementSpec
            , serializationPropertyTests
            -- Daemon tests
            , daemonStateSpec
            , daemonProtocolSpec
            , daemonProcessSpec
            , daemonServerSpec
            , daemonConfigWatcherSpec
            ]
