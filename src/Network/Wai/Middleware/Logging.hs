{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
module Network.Wai.Middleware.Logging
  ( logging
  , loggingApacheLike
  , apacheLikeLog
  ) where

import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString       as BS
import qualified Data.Text             as T
import qualified Data.Text.Encoding    as TE
import qualified Network.HTTP.Types    as HT
import qualified Control.Exception     as E
import qualified Network.Wai           as W
import Network.Wai
import Control.Exception
import Data.IORef
import Control.Applicative ((<|>))
import Data.Bits (shift, (.&.))
import Data.Word (Word32)
import Data.Maybe (fromMaybe)
import Data.Either (either)
import Network.Socket (SockAddr(..))
import System.ByteOrder (ByteOrder(..), byteOrder)
import Text.Printf (printf)

-- If we simply called callback function on response and exception,
-- callback could be called on both case. So we should make sure
-- callback is only called once.
logging :: (Request -> Either SomeException Response -> IO ()) -> Middleware
logging cb' app req resF = do
  cb <- onlyOnce cb'
  handle (errorHandler cb) $ app req $ \res -> do
    _ <- try @SomeException $ cb req (Right res)
    resF res
  where
    errorHandler cb e = do
      _ <- try @SomeException $ cb req (Left e)
      throwIO e

    onlyOnce f = do
      v <- newIORef False
      pure $ \req res -> do
        v' <- atomicModifyIORef v (\v -> (True, v))
        if v'
          then pure ()
          else f req res

loggingApacheLike :: (T.Text -> IO ()) -> Middleware
loggingApacheLike cb =
  logging (\req res -> cb $ TE.decodeUtf8 $ apacheLikeLog req res)

-- This really should be another package!!!

-- Apache style log without timestamp
-- Also we can't get the size of response easily so just "-"
-- https://hackage.haskell.org/package/wai-logger-2.3.5/docs/src/Network.Wai.Logger.Apache.html#getSourceIP
apacheLikeLog :: W.Request -> Either E.SomeException W.Response -> BS.ByteString
apacheLikeLog req resE = ip
  <> " - - \""
  <> method
  <> " "
  <> path
  <> " "
  <> httpVer
  <> "\" "
  <> rcode
  <> " "
  <> rsize
  <> " \""
  <> referer
  <> "\" \""
  <> ua
  <> "\""
  where
    ip      = fromMaybe (ipFromSocket req) (ipFromHeader req)
    method  = W.requestMethod req
    path    = W.rawPathInfo req <> W.rawQueryString req
    httpVer = BS8.pack $ show $ W.httpVersion req
    rcode   = BS8.pack $ either (const "E") (show . HT.statusCode . W.responseStatus) resE
    rsize   = "-"
    referer = fromMaybe "" $ W.requestHeaderReferer req
    ua      = fromMaybe "" $ W.requestHeaderUserAgent req

    ipFromSocket =
      BS8.pack . showSockAddr . W.remoteHost

    ipFromHeader req = a <|> b
      where
        headers = W.requestHeaders req
        a = lookup "x-real-ip" headers
        b = lookup "x-forwarded-for" headers

-- Copied from https://hackage.haskell.org/package/wai-logger-2.3.5/docs/src/Network.Wai.Logger.IP.html#showSockAddr

showIPv4 :: Word32 -> Bool -> String
showIPv4 w32 little
    | little    = show b1 ++ "." ++ show b2 ++ "." ++ show b3 ++ "." ++ show b4
    | otherwise = show b4 ++ "." ++ show b3 ++ "." ++ show b2 ++ "." ++ show b1
  where
    t1 = w32
    t2 = shift t1 (-8)
    t3 = shift t2 (-8)
    t4 = shift t3 (-8)
    b1 = t1 .&. 0x000000ff
    b2 = t2 .&. 0x000000ff
    b3 = t3 .&. 0x000000ff
    b4 = t4 .&. 0x000000ff

showIPv6 :: (Word32,Word32,Word32,Word32) -> String
showIPv6 (w1,w2,w3,w4) =
    printf "%x:%x:%x:%x:%x:%x:%x:%x" s1 s2 s3 s4 s5 s6 s7 s8
  where
    (s1,s2) = split16 w1
    (s3,s4) = split16 w2
    (s5,s6) = split16 w3
    (s7,s8) = split16 w4
    split16 w = (h1,h2)
      where
        h1 = shift w (-16) .&. 0x0000ffff
        h2 = w .&. 0x0000ffff

-- | Convert 'SockAddr' to 'NumericAddress'. If the address is
--   IPv4-embedded IPv6 address, the IPv4 is extracted.
showSockAddr :: SockAddr -> String
-- HostAddr is network byte order.
showSockAddr (SockAddrInet _ addr4)                       = showIPv4 addr4 (byteOrder == LittleEndian)
-- HostAddr6 is host byte order.
showSockAddr (SockAddrInet6 _ _ (0,0,0x0000ffff,addr4) _) = showIPv4 addr4 False
showSockAddr (SockAddrInet6 _ _ (0,0,0,1) _)              = "::1"
showSockAddr (SockAddrInet6 _ _ addr6 _)                  = showIPv6 addr6
showSockAddr _                                            = "unknownSocket"
