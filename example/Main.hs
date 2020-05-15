import Network.Wai.Application.Static (staticApp, defaultWebAppSettings)
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Network.Wai.Middleware.RateLimit (
    LeakyBucketSpec (..), RateLimitState (..), fromHz, inMemory, rateLimitMiddleware)

import qualified StmContainers.Map as STMMap


main :: IO ()
main = do
    -- Set up the in-memory rate-limiting state that persists across requests.
    authClaimRateLimits <- STMMap.newIO
    ipRateLimits <- STMMap.newIO

    let authClaimLeakyBucketSpec = LeakyBucketSpec 1 (fromHz 1)
        ipLeakyBucketSpec = LeakyBucketSpec 1 (fromHz 1)

        rateLimitState = RateLimitState
            { withAuthclaimBucket = inMemory (const authClaimLeakyBucketSpec) authClaimRateLimits
            , withIpBucket = inMemory (const ipLeakyBucketSpec) ipRateLimits
            }
        middleware = rateLimitMiddleware rateLimitState Nothing
    run 8080 (logStdoutDev $ middleware $ staticApp $ defaultWebAppSettings ".")