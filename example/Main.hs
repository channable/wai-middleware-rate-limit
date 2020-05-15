-- This file contains a minimal example of how to use the rate-limiting middleware.
-- We use an STMMap to persist the rate-limiting state across requests.
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

import Data.Hashable (Hashable (..))
import Data.List (find)
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8)

import Network.HTTP.Types (hAuthorization)
import Network.Wai (Request, requestHeaders)
import Network.Wai.Application.Static (staticApp, defaultWebAppSettings)
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Network.Wai.Middleware.RateLimit (
    LeakyBucketSpec (..), RateLimitState (..), fromHz, inMemory, rateLimitMiddleware)

import qualified StmContainers.Map as STMMap

-- your token type must be deriving Hashable, since the hash of the token data will
-- be used as the key for the rate limiting hash map
newtype MyToken = MyToken {getText :: Text} deriving (Eq, Ord, Show, Hashable)

extractToken :: Request -> Maybe MyToken
extractToken request = maybe Nothing (Just . MyToken . decodeUtf8 . snd) maybeHeader
  where
    maybeHeader = find ((== hAuthorization) . fst) (requestHeaders request)


main :: IO ()
main = do
    -- Set up the in-memory rate-limiting state that persists across requests.
    -- This STMMap is used to rate-limit based on the authentication token of a client
    authClaimRateLimits <- STMMap.newIO
    -- This STMMap is used to rate-limit based on the IP address of a client
    ipRateLimits <- STMMap.newIO

    -- set the bucket size to 1 and the leak rate to 1 Hz, i.e. 1 request per second is allowed
    let authClaimLeakyBucketSpec = LeakyBucketSpec 1 (fromHz 1)
        ipLeakyBucketSpec = LeakyBucketSpec 1 (fromHz 1)

        -- set up the data structure keeping track of both types of buckets
        rateLimitState = RateLimitState
            { withAuthclaimBucket = inMemory (const authClaimLeakyBucketSpec) authClaimRateLimits
            , withIpBucket = inMemory (const ipLeakyBucketSpec) ipRateLimits
            }

        -- this is the actual rate-limiting middleware. We pass 'Nothing' here to keep this
        -- example simple. In a real application, that has authenticated users, we would pass
        -- the concrete token type here.
        middleware = rateLimitMiddleware rateLimitState Nothing

        app = logStdoutDev $ middleware $ staticApp $ defaultWebAppSettings "."

    -- Start a web server, serving static files from the local directory.
    -- Requests are rate-limited based on IP address, according to the "ipLeakyBucketSpec" above.
    -- You can test this by sending a few requests with "curl localhost:8080" and noticing the
    -- "429 Too Many Requests" responses, that are being logged to stdout.
    run 8080 app