-- We need this extension because of MonadState constraints below; without it
-- only, type variables could appear in constraints, not actual types such as
-- LeakyBucket.
{-# LANGUAGE FlexibleContexts #-}

module Network.Wai.Middleware.RateLimit.RateLimit
  ( -- * Rate limiting actions
    recordAccess
  , resetLimit
  , queryLimit

    -- * In-memory state
  , inMemory
  ) where

import Control.Concurrent.STM (atomically)
import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.State (MonadState, State, runState)
import Data.Foldable (traverse_)
import Data.Hashable (Hashable)
import Data.Maybe (fromMaybe, isJust)
import Data.Time.Clock (UTCTime)
import Prelude hiding (map)
import StmContainers.Map (Map)

import qualified Control.Monad.State as State
import qualified StmContainers.Map as Map

import Network.Wai.Middleware.RateLimit.LeakyBucket (
  LeakyBucket (..), LeakyBucketSpec, empty, insert, leakUntil)


-- | Record that a resource is going to be accessed, and return whether this is
-- allowed.
recordAccess :: MonadState LeakyBucket m => UTCTime -> m Bool
recordAccess now = do
  oldBucket <- State.get
  let newBucket = insert now oldBucket
  traverse_ State.put newBucket
  pure . isJust $ newBucket

-- | Reset a rate limit.
resetLimit :: MonadState LeakyBucket m => m ()
resetLimit = State.modify $ \lb -> lb { lbContent = 0 }

-- | What is the current rate limit?
queryLimit :: MonadState LeakyBucket m => UTCTime -> m LeakyBucket
queryLimit = State.gets . leakUntil

--------------------------------------------------------------------------------

-- | Run an action on a single rate limit over in-memory state. The 'MonadIO'
-- constraint is used for STM.
inMemory
  :: (Hashable k, Eq k, MonadIO m)
  => (k -> LeakyBucketSpec)
  -> Map k LeakyBucket
  -> State LeakyBucket a
  -> k
  -> m a
inMemory defaultBucketSpec map action key = do
  let defaultBucket = empty (defaultBucketSpec key)
  liftIO . atomically $ do
    oldBucket <- fromMaybe defaultBucket <$> Map.lookup key map
    let (result, newBucket) = runState action oldBucket
    when (newBucket /= oldBucket) $
      Map.insert newBucket key map
    pure result
