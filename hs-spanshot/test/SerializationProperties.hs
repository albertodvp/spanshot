{-# OPTIONS_GHC -Wno-orphans #-}

module SerializationProperties (serializationPropertyTests) where

import Data.Aeson qualified as Aeson
import Data.Text (Text)
import Data.Text qualified as T
import Test.QuickCheck (Arbitrary (..), Gen, elements, listOf)
import Test.QuickCheck.Instances ()
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty, withMaxSuccess)

import Fixtures (arbitraryUTCTime)
import Types (CollectEvent (CollectEvent), DetectionRule (RegexRule), SpanShot (SpanShot, postWindow, preWindow))

printableChars :: [Char]
printableChars = ['a' .. 'z'] ++ ['A' .. 'Z'] ++ ['0' .. '9'] ++ " .,!?-_"

arbitraryNonEmptyString :: Gen String
arbitraryNonEmptyString = do
    len <- elements @Integer [1 .. 50]
    mapM (\_ -> elements printableChars) [1 .. len]

arbitraryNonEmptyText :: Gen Text
arbitraryNonEmptyText = T.pack <$> arbitraryNonEmptyString

instance Arbitrary DetectionRule where
    arbitrary = RegexRule <$> arbitraryNonEmptyString

instance Arbitrary CollectEvent where
    arbitrary =
        CollectEvent
            <$> arbitraryNonEmptyText
            <*> arbitrary
            <*> arbitraryUTCTime
            <*> arbitraryNonEmptyText

instance Arbitrary SpanShot where
    arbitrary =
        SpanShot
            <$> arbitrary
            <*> listOf arbitrary
            <*> listOf arbitrary
            <*> listOf arbitrary
            <*> arbitraryUTCTime

jsonRoundTrip :: (Eq a, Aeson.ToJSON a, Aeson.FromJSON a) => a -> Bool
jsonRoundTrip value = Aeson.decode (Aeson.encode value) == Just value

prop_detectionRule_roundtrip :: DetectionRule -> Bool
prop_detectionRule_roundtrip = jsonRoundTrip

prop_collectEvent_roundtrip :: CollectEvent -> Bool
prop_collectEvent_roundtrip = jsonRoundTrip

prop_spanShot_roundtrip :: SpanShot -> Bool
prop_spanShot_roundtrip = jsonRoundTrip

prop_spanShot_windows_preserve_length :: SpanShot -> Bool
prop_spanShot_windows_preserve_length snapshot =
    let encoded = Aeson.encode snapshot
        decoded = Aeson.decode encoded :: Maybe SpanShot
     in case decoded of
            Just s ->
                length (preWindow s) == length (preWindow snapshot)
                    && length (postWindow s) == length (postWindow snapshot)
            Nothing -> False

serializationPropertyTests :: TestTree
serializationPropertyTests =
    testGroup
        "Serialization Properties (QuickCheck)"
        [ testProperty "DetectionRule round-trips through JSON" $
            withMaxSuccess 1000 prop_detectionRule_roundtrip
        , testProperty "CollectEvent round-trips through JSON" $
            withMaxSuccess 1000 prop_collectEvent_roundtrip
        , testProperty "SpanShot round-trips through JSON" $
            withMaxSuccess 1000 prop_spanShot_roundtrip
        , testProperty "SpanShot preserves window list lengths" $
            withMaxSuccess 1000 prop_spanShot_windows_preserve_length
        ]
