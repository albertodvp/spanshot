module Fixtures (
    mockTime,
    mockEvent,
    mockEventAtTime,
    arbitraryUTCTime,
) where

import Data.Text (Text)
import Data.Time (UTCTime (..), fromGregorian, secondsToDiffTime)
import Test.QuickCheck (Gen, choose)

import Types (CollectEvent (CollectEvent, line, readAtUtc, sessionOrderId, source))

{- | Create a deterministic UTC time for testing.
Uses the Apollo 11 moon landing date (1969-07-20) as a memorable fixed date.
The seconds parameter becomes the time-of-day offset from midnight.
This ensures all test timestamps are reproducible and clearly artificial.
-}
mockTime :: Integer -> UTCTime
mockTime seconds = UTCTime (fromGregorian 1969 7 20) (secondsToDiffTime seconds)

{- | Create a test event where orderId doubles as the timestamp in seconds.
This makes time-based tests intuitive and self-documenting:
  mockEvent 10 "ERROR"  -- event at t=10s with orderId=10
  mockEvent 15 "INFO"   -- event at t=15s with orderId=15
The time difference between events equals the orderId difference.
-}
mockEvent :: Int -> Text -> CollectEvent
mockEvent orderId content =
    CollectEvent
        { source = "test.log"
        , sessionOrderId = orderId
        , readAtUtc = mockTime (fromIntegral orderId)
        , line = content
        }

{- | Create a test event at a specific time with a fixed orderId of 0.
Use when you need precise time control but don't care about ordering.
-}
mockEventAtTime :: Text -> UTCTime -> CollectEvent
mockEventAtTime content time =
    CollectEvent
        { source = "test.log"
        , sessionOrderId = 0
        , readAtUtc = time
        , line = content
        }

{- | QuickCheck generator for arbitrary UTC times.
Used in property-based serialization tests to ensure JSON round-trips work
for any valid timestamp. Generates times between 2000-2100 to avoid edge cases.
-}
arbitraryUTCTime :: Gen UTCTime
arbitraryUTCTime = do
    year <- choose (2000, 2100)
    month <- choose (1, 12)
    day <- choose (1, 28)
    seconds <- choose (0, 86400 - 1)
    pure $ UTCTime (fromGregorian year month day) (secondsToDiffTime seconds)
