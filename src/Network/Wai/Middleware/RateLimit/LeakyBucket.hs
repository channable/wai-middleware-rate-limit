module Data.LeakyBucket
  ( LeakyBucketSpec (..)
  , LeakyBucket (..)
  , empty
  , insert
  , leakUntil
  ) where

import Data.Time.Calendar (Day (..))
import Data.Time.Clock (UTCTime (..), diffUTCTime)

import Data.Time.Frequency (Frequency, mulDxF)


-- | A capacity and a leak rate.
data LeakyBucketSpec =
  LeakyBucketSpec !Word !Frequency
  deriving (Eq, Show)

-- | A leaky bucket.
data LeakyBucket = LeakyBucket
  { lbCapacity :: !Word
  , lbContent  :: !Word
  , lbLastLeak :: !UTCTime
  , lbLeakRate :: !Frequency
  } deriving (Eq, Show)

-- | An empty bucket.
empty :: LeakyBucketSpec -> LeakyBucket
empty (LeakyBucketSpec capacity leakRate) =
  LeakyBucket capacity 0 lastLeak leakRate
  where -- It does not matter what we use here, because it will be overwritten
        -- on the first insert anyway.
        lastLeak = UTCTime (ModifiedJulianDay 0) 0

-- | Insert an item into the bucket. Fail with 'Nothing' if the bucket is full.
insert :: UTCTime -> LeakyBucket -> Maybe LeakyBucket
insert now = insert' . leakUntil now
  where insert' lb
          | lbContent lb >= lbCapacity lb = Nothing
          | otherwise = Just $ lb { lbContent = lbContent lb + 1 }

-- | Leak until the given time.
leakUntil :: UTCTime -> LeakyBucket -> LeakyBucket
leakUntil now lb = lb { lbContent = lbContent', lbLastLeak = now }
  where dcontent = floor ((now `diffUTCTime` lbLastLeak lb) `mulDxF` lbLeakRate lb)
        lbContent' = if lbContent lb < dcontent then 0 else lbContent lb - dcontent
