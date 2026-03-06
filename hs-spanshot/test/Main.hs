module Main (main) where

import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.Hspec (testSpec)

import CaptureStreamSpec (captureStreamTests)
import CaptureTypesSpec (captureTypesTests)
import CollectionSpec (collectionTests)
import ConfigSpec (configTests)
import DetectionSpec (detectionTests)
import SerializationProperties (serializationPropertyTests)
import StorageSpec (storageTests)
import WindowManagementSpec (windowManagementTests)

main :: IO ()
main = do
    collectionSpec <- testSpec "Collection Tests" collectionTests
    captureTypesSpec <- testSpec "Capture Types Tests" captureTypesTests
    captureStreamSpec <- testSpec "Capture Stream Tests" captureStreamTests
    configSpec <- testSpec "Config Tests" configTests
    detectionSpec <- testSpec "Detection Tests" detectionTests
    storageSpec <- testSpec "Storage Tests" storageTests
    windowManagementSpec <- testSpec "Window Management Tests" windowManagementTests

    defaultMain $
        testGroup
            "SpanShot Tests"
            [ collectionSpec
            , captureTypesSpec
            , captureStreamSpec
            , configSpec
            , detectionSpec
            , storageSpec
            , windowManagementSpec
            , serializationPropertyTests
            ]
