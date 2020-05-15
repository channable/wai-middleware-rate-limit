{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

-- Necessary for withLeakyBucket to be agnostic of return type. Without this,
-- we would have to specify a specific return type, which would not work with
-- multiple uses of the function.
{-# LANGUAGE RankNTypes #-}

module Network.Wai.Middleware.RateLimit
  ( RateLimitState (..)
  , RateLimitMiddleware
  , rateLimitMiddleware

  -- re-exports
  , LeakyBucketSpec (..)
  , fromHz
  , inMemory
  ) where

import Control.Monad.State (State)
import Data.Aeson (ToJSON, encode)
import Data.Hashable (Hashable (..))
import Data.Time.Clock (getCurrentTime)
import GHC.Generics (Generic)

import qualified Data.ByteString.Lazy as LBS
import qualified Network.HTTP.Types as HTTPTypes
import qualified Network.Wai as Wai

import Network.Wai.Middleware.RateLimit.Frequency (fromHz)
import Network.Wai.Middleware.RateLimit.LeakyBucket (
    LeakyBucket (..), LeakyBucketSpec (..))
import Network.Wai.Middleware.RateLimit.RateLimit (
    inMemory, queryLimit, recordAccess)

import qualified Network.Wai.Middleware.RateLimit.IP as IP


-- | Functions for updating the rate limit state.
data RateLimitState authClaim = RateLimitState
  { withAuthclaimBucket ::forall a . State LeakyBucket a -> authClaim -> IO a
    -- ^ Update the state for the given token.
  , withIpBucket :: forall a. State LeakyBucket a -> IP.Address -> IO a
    -- ^ Update the state for the given IP address.
  }

-- | We need to pass extra data to the wrapper middleware, so we cannot just
-- use 'Wai.Middleware' here.
type RateLimitMiddleware authClaim = Maybe authClaim -> Wai.Application -> Wai.Application

-- | Add rate limiting to an application.
rateLimitMiddleware :: RateLimitState authClaim -> RateLimitMiddleware authClaim
rateLimitMiddleware state authClaim app request respond =
  let
    checkIp = checkIpRateLimit state app request respond
    checkAuthclaim tok = checkAuthclaimRateLimit state tok app request respond
  in
    case Wai.pathInfo request of
      ["rate_limits"] -> respond =<< displayRateLimit state authClaim
      -- Note, that if there is no authClaim, the IP rate limits will be applied.
      _ -> maybe checkIp checkAuthclaim authClaim

displayRateLimit :: RateLimitState authClaim -> Maybe authClaim -> IO Wai.Response
displayRateLimit _ Nothing = pure $ jsonResponse HTTPTypes.status401 ()
displayRateLimit rlState (Just authClaim) = do
  now <- getCurrentTime
  bucket <- withAuthclaimBucket rlState (queryLimit now) authClaim
  let requestsLeft = lbCapacity bucket - lbContent bucket
  pure $ jsonResponse HTTPTypes.status200 requestsLeft

checkAuthclaimRateLimit :: RateLimitState authClaim -> authClaim -> Wai.Middleware
checkAuthclaimRateLimit rlState authClaim app request respond = do
  now <- getCurrentTime
  ok <- withAuthclaimBucket rlState (recordAccess now) authClaim
  if ok then app request respond
        else respond tooManyRequestsResponse

checkIpRateLimit :: RateLimitState authClaim -> Wai.Middleware
checkIpRateLimit rlState app request respond = do
  case IP.fromWaiRequest request of
    -- If we can't find out what the client address is,
    -- then something is misconfigured and we should
    -- allow the request. (Fail open)
    Nothing -> allow
    Just addr -> do
      now <- getCurrentTime
      ok <- withIpBucket rlState (recordAccess now) addr
      if ok then allow
            else block
  where
    allow = app request respond
    block = respond tooManyRequestsResponse


tooManyRequestsResponse :: Wai.Response
tooManyRequestsResponse = jsonResponse (HTTPTypes.Status 429 "Too Many Requests") ()

jsonResponseLBS :: HTTPTypes.Status -> LBS.ByteString -> Wai.Response
jsonResponseLBS status = Wai.responseLBS status [(HTTPTypes.hContentType, "application/json")]

jsonResponse :: ToJSON a => HTTPTypes.Status -> a -> Wai.Response
jsonResponse status value = jsonResponseLBS status (encode value)