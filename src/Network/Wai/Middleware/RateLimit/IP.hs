-- So we can have an Orphan instance of @Hashable IPv6@.
{-# OPTIONS_GHC -Wno-orphans #-}

{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Network.Wai.Middleware.RateLimit.IP
  ( Address (..)
  , fromWaiRequest
  ) where

import Control.Applicative ((<|>))
import Data.Hashable (Hashable (..))
import Data.WideWord.Word128 (Word128 (..))
import Data.Word (Word32)
import GHC.Generics (Generic)

import qualified Data.Text.Encoding as Text
import qualified Net.IPv4 as IPv4
import qualified Net.IPv6 as IPv6
import qualified Network.HTTP.Types as HTTP
import qualified Network.Socket as Socket
import qualified Network.Wai as Wai


-- Orphan instance for Hashable IPv6. The `ip` package does provide
-- a hashable instance for IPv4 already.
instance Hashable IPv6.IPv6 where
  hashWithSalt s ip =
    let Word128 hi lo = IPv6.getIPv6 ip
    in s `hashWithSalt` hi `hashWithSalt` lo


data Address
  = IPv4 IPv4.IPv4
  | IPv6 IPv6.IPv6
  deriving (Eq, Generic, Hashable)


-- Parse an @Address@ from a @Wai.Request@.
fromWaiRequest :: Wai.Request -> Maybe Address
fromWaiRequest req = fromRequestHeaders (Wai.requestHeaders req) <|> fromSockAddr (Wai.remoteHost req)


-- Parse an address from the @X-Real-IP@ HTTP header. In production, this
-- is set by Nginx to ensure that warpmachine can know the address of the
-- actual client and not `127.0.0.1`.
fromRequestHeaders :: HTTP.RequestHeaders -> Maybe Address
fromRequestHeaders headers = do
  header <- lookup "X-Real-IP" headers
  headerDecoded <- rightToMaybe $ Text.decodeUtf8' header
  IPv4 <$> IPv4.decode headerDecoded <|> IPv6 <$> IPv6.decode headerDecoded


rightToMaybe :: Either a b -> Maybe b
rightToMaybe e = case e of
  Left _ -> Nothing
  Right b -> Just b


-- Turn a Word32 into an @Address@
fromWord :: Word32 -> Address
fromWord = IPv4 . IPv4.IPv4


-- Turn a tuple of @Word32@s into an @Address@
fromWords :: (Word32, Word32, Word32, Word32) -> Address
fromWords = IPv6 . IPv6.fromTupleWord32s


-- Parse an address from the given SockAddr. This can fail because the
-- socket might not be an IP socket (it can be a unix socket).
fromSockAddr :: Socket.SockAddr -> Maybe Address
fromSockAddr socketAddr = case socketAddr of
  Socket.SockAddrInet _port address -> Just $ fromWord address
  Socket.SockAddrInet6 _port _flowInfo address _scopeId -> Just $ fromWords address
  -- We don't make warpmachine listen on a Unix socket, so this should
  -- be a dead codepath.
  Socket.SockAddrUnix _str -> Nothing
