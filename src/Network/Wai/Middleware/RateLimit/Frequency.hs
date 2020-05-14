module Network.Wai.Middleware.RateLimit.Frequency
  ( Frequency (..)
  , RequestsPerMinute (..)
  , fromRequestsPerMinute
  , fromHz
  , mulDxF
  ) where

import Data.Time.Clock (NominalDiffTime)


-- | A frequency is represented by its reciprocal. For example, @Frequency 0.2@
-- represents a frequency of 5 Hz.
newtype Frequency = Frequency { freqRecip :: NominalDiffTime }
  deriving (Eq, Ord, Show)

newtype RequestsPerMinute = RequestsPerMinute Integer

-- | @fromHz n = n Hz@.
fromHz :: Double -> Frequency
fromHz = Frequency . fromRational . recip . toRational

fromRequestsPerMinute :: RequestsPerMinute -> Frequency
fromRequestsPerMinute (RequestsPerMinute n) = fromHz $  fromIntegral n / 60

-- | Multiplying a duration (s) by a frequency (1/s) gives a number.
mulDxF :: NominalDiffTime -> Frequency -> Rational
mulDxF d f = toRational $ d / freqRecip f
