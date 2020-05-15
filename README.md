# WAI Rate-limiting Middleware

Easily add rate-limiting to your WAI applications based on the
[leaky bucket algorithm](https://en.wikipedia.org/wiki/Leaky_bucket).

This library allows you to rate-limit incoming HTTP requests based on
- the IP address of the client
- the authentication token of the client (currently tied to `Web.JWT`-style tokens)

If a request with an authentication token comes in, the token rate-limits will be applied. If an unauthenticated request comes in the IP rate-limiting will be applied.

## Usage

There is a minimal, self-contained example in `example/Main.hs` that shows how to rate-limit on IP.
You have to define `LeakyBucketSpec` which specifies the capacity of the bucket, and the leak rate.
E.g. a `LeakyBucketSpec 10 (fromHz 1)` would be a bucket with a capacity of 10 requests, and a leak rate of 1 request per second. A client could thus send 10 requests in a burst, and subsequently one request per second without getting rate-limited.
When sending more requests than that we return a "429 Too Many Requests" status code.

Minimal usage example:

```haskell
import Network.Wai.Application.Static (staticApp, defaultWebAppSettings)
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Network.Wai.Middleware.RateLimit (
    LeakyBucketSpec (..), RateLimitState (..), fromHz, inMemory, rateLimitMiddleware)

import qualified StmContainers.Map as STMMap


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

    -- Start a web server, serving static files from the local directory.
    -- Requests are rate-limited based on IP address, according to the "ipLeakyBucketSpec" above.
    -- You can test this by sending a few requests with "curl localhost:8080" and noticing the
    -- "429 Too Many Requests" responses, that are being logged to stdout.
    run 8080 (logStdoutDev $ middleware $ staticApp $ defaultWebAppSettings ".")
```

## Implementation

This library implements the [leaky bucket algorithm](https://en.wikipedia.org/wiki/Leaky_bucket).
Internally, we use an `STMMap` from [`stm-containers`](https://github.com/nikita-volkov/stm-containers) to keep track of the rate-limiting state.
There is one `STMMap` to keep track of IP-based state, and another `STMMap` to keep track of token-based state.


## Limitations

The library currently only supports in-memory storage of the rate-limiting state. A restart of the
server process, will thus lose all rate-limiting state.

Another limitation is that only `Web.JWT`-style authentication tokens are supported, instead of
arbitrary token types.

## Building

The library can be built with `stack build`.

The example application can be run with: `stack run wai-middleware-rate-limit-example`