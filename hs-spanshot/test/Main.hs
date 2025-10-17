module Main (main) where

import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.Hspec (testSpec)

import CaptureTypesSpec (captureTypesTests)
import CollectionSpec (collectionTests)
import DetectionSpec (detectionTests)
import SerializationProperties (serializationPropertyTests)

main :: IO ()
main = do
    collectionSpec <- testSpec "Collection Tests" collectionTests
    captureTypesSpec <- testSpec "Capture Types Tests" captureTypesTests
    detectionSpec <- testSpec "Detection Tests" detectionTests

    defaultMain $
        testGroup
            "SpanShot Tests"
            [ collectionSpec
            , captureTypesSpec
            , detectionSpec
            , serializationPropertyTests
            ]
