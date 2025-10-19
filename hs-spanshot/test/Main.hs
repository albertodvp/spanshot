module Main (main) where

import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.Hspec (testSpec)

import CaptureStreamSpec (captureStreamTests)
import CaptureTypesSpec (captureTypesTests)
import CollectionSpec (collectionTests)
import DetectionSpec (detectionTests)
import SerializationProperties (serializationPropertyTests)
import WindowManagementSpec (windowManagementTests)

main :: IO ()
main = do
    collectionSpec <- testSpec "Collection Tests" collectionTests
    captureTypesSpec <- testSpec "Capture Types Tests" captureTypesTests
    captureStreamSpec <- testSpec "Capture Stream Tests" captureStreamTests
    detectionSpec <- testSpec "Detection Tests" detectionTests
    windowManagementSpec <- testSpec "Window Management Tests" windowManagementTests

    defaultMain $
        testGroup
            "SpanShot Tests"
            [ collectionSpec
            , captureTypesSpec
            , captureStreamSpec
            , detectionSpec
            , windowManagementSpec
            , serializationPropertyTests
            ]
