module Fixtures (
    mockTime,
    mockEvent,
    mockEventAtTime,
    arbitraryUTCTime,
) where

import Data.Text (Text)
import Data.Text qualified as T
import Data.Time (UTCTime (..), fromGregorian, secondsToDiffTime)
import Test.QuickCheck (Gen, choose)

import Types (CollectEvent (..))

mockTime :: Integer -> UTCTime
mockTime seconds = UTCTime (fromGregorian 2025 10 16) (secondsToDiffTime seconds)

mockEvent :: Int -> Text -> CollectEvent
mockEvent orderId content =
    CollectEvent
        { source = T.pack "test.log"
        , sessionOrderId = orderId
        , readAtUtc = mockTime (fromIntegral orderId)
        , line = content
        }

mockEventAtTime :: Text -> UTCTime -> CollectEvent
mockEventAtTime content time =
    CollectEvent
        { source = T.pack "test.log"
        , sessionOrderId = 0
        , readAtUtc = time
        , line = content
        }

arbitraryUTCTime :: Gen UTCTime
arbitraryUTCTime = do
    year <- choose (2000, 2100)
    month <- choose (1, 12)
    day <- choose (1, 28)
    seconds <- choose (0, 86400 - 1)
    pure $ UTCTime (fromGregorian year month day) (secondsToDiffTime seconds)
