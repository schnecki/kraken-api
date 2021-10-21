{-# LANGUAGE CPP                        #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MagicHash                  #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE UndecidableSuperClasses    #-}
module Request.Kraken.Class
  ( Request(..)
  , Session(..)
  , KrakenConfig (..)
  -- , krakenConfigPracticeAccount
  , krakenConfigTradeAccount
  -- , baseUrl
  , withApiSignNonce
  , mkApiSignature
  , additionalParams
  , headerContentTypeJson
  , headerContentDispositionFile
  , headerContentTypeMultipart
  , headerRFC3339DatetimeFormat
  , runSafeReqM
  , SafeReqM (..)
  ) where

import           Control.Monad.Base
import           Control.Monad.Except
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Control
import           Control.Monad.Trans.Reader
import           Control.Monad.Trans.State
import qualified Crypto.Hash.SHA256             as SHA256
import qualified Crypto.Hash.SHA512             as SHA512
import           Data.Bifunctor                 (bimap)
import qualified Data.ByteString                as B
import qualified Data.ByteString                as BS
import           Data.ByteString.Builder
import qualified Data.ByteString.Internal       as B
import           Data.Char
import           Data.List                      (foldl')
import           Data.Text.Internal.Unsafe.Char
import qualified Data.Word                      as W
import           GHC.Exts
import           GHC.Show                       (showLitChar)
import           Numeric                        (showHex)
import           Text.Printf
-- import qualified Data.ByteString.Lazy        as BS
import qualified Data.ByteString.Base16         as B16
import qualified Data.ByteString.Base64         as B64
import qualified Data.ByteString.Lazy.Char8     as BL
import qualified Data.CaseInsensitive           as CI
import           Data.List                      (find)
import           Data.Maybe                     (maybe)
import           Data.Proxy
import           Data.Text                      (Text)
import qualified Data.Text                      as T
import qualified Data.Text.Encoding             as E
import qualified Data.Text.Encoding.Base64      as T64
import           Data.Time.Clock.POSIX          (getPOSIXTime)
import           Network.HTTP.Base              (urlEncodeVars)
import qualified Network.HTTP.Client            as C

import           Debug.Trace

import           ApiMaker

version :: Text
version = "0"

type APIKey = B.ByteString
type PrivateAPIKey = B.ByteString
type OTP = B.ByteString

-- krakenConfigPracticeAccount :: AccessToken -> KrakenConfig
-- krakenConfigPracticeAccount = KrakenConfig baseUrlPractice streamUrlPractice

krakenConfigTradeAccount :: APIKey -> PrivateAPIKey -> Maybe OTP -> KrakenConfig
krakenConfigTradeAccount apiKey prApiKey mOtp =
  KrakenConfig baseUrlTrade apiKey (B64.decodeBase64Lenient prApiKey) mOtp

data KrakenConfig =
  KrakenConfig
    { baseUrl           :: Url 'Https
    , apiKey            :: APIKey
    , privateKeyDecoded :: B.ByteString
    , otp               :: Maybe OTP
    }


additionalParams :: KrakenConfig -> [Option 'Https]
additionalParams config = [] -- header "Content-Type" "application/x-www-form-urlencoded; charset=utf-8"]

-- | Sign the API call with the given API-Key, add nonce and OTP if given.
withApiSignNonce :: KrakenConfig -> Option 'Https -> IO (Option 'Https)
withApiSignNonce cfg ops = (<> ops') <$> mkNonce
  where ops' = ops <> mkApiKey cfg <> mkOtp cfg

mkApiKey :: KrakenConfig -> Option 'Https
mkApiKey cfg = header "API-Key" (apiKey cfg)

mkOtp :: KrakenConfig -> Option 'Https
mkOtp cfg = maybe mempty (header "otp") (otp cfg)

mkNonce :: IO (Option 'Https)
mkNonce = do
  time <- unixTimestamp
  let headerOp = header "nonce" . E.encodeUtf8 . T.pack $ time
  return $ "nonce"  =: time <> headerOp
  where unixTimestamp = return "1634761768491" -- show . round . (* 1000) <$> getPOSIXTime

-- HMAC-SHA512 of (URI path + SHA256(nonce + POST data)) and base64 decoded secret API key
mkApiSignature :: KrakenConfig -> r -> C.Request -> IO C.Request
mkApiSignature cfg _ r = return $
  trace ("api-sign: " ++ show (E.encodeUtf8 hash))
  trace ("headers: " ++ show (headersData))
  attachHeader "API-Key" (apiKey cfg) $
  attachHeader "API-Sign" (E.encodeUtf8 hash) $
  r {C.requestHeaders = headersData}
  where
    hash = B64.encodeBase64 $ SHA512.hmac (privateKeyDecoded cfg) message
    headers = C.requestHeaders r
    headersData = filter ((/= CI.mk "api-key") . fst) headers
    nNonce = CI.mk "nonce"
    nonce = maybe (error $ "Could not find nonce in request " ++ show r ++ ", but expected a nonce in the headers!") snd $ find ((== nNonce) . fst) headers
    message = uriPath <> sha256Data
    sha256Data = SHA256.hash utf8EncodedDataStr
    uriPath = C.path r
    encodedDataHeaders = map (\(x, y) -> (fromBS $ CI.foldedCase x, fromBS y)) headersData
    utf8EncodedDataStr = nonce <> E.encodeUtf8 (T.pack $ urlEncodeVars encodedDataHeaders)
    fromBS = T.unpack . E.decodeUtf8


byteS :: W.Word8 -> ShowS
byteS b
  | b < 128 = ((asciiLs !! fromIntegral b) <>)
  | otherwise = ("\\x" <>) . showHex b

bytesS :: BS.ByteString -> ShowS
bytesS b | B.length b == 1 = byteS $ B.head b
         | otherwise = foldl' ((. byteS) . (.)) id $ B.unpack b

showByteString :: BS.ByteString -> String
showByteString b = bytesS b ""

baseUrlTrade :: Url 'Https
baseUrlTrade = https "api.kraken.com" /: version

-- baseUrlPractice :: Url 'Https
-- baseUrlPractice = https "api.kraken.com" /: version

-- streamUrlTrade :: Url 'Https
-- streamUrlTrade = https "stream-fxtrade.kraken.com" /: version

-- streamUrlPractice :: Url 'Https
-- streamUrlPractice = https "stream-fxpractice.kraken.com" /: version

-- TODO: see http://hdiff.luite.com/cgit/kraken-rest-api/commit?id=0.4.0 for streaming

-- headerBearer :: KrakenConfig -> Option 'Https
-- headerBearer config =
--   -- header "Authorization" ("Bearer " <> accessToken config)
--   header "API-Key" ("Bearer " <> apiKey config) <>
--   header "API-Key" ("Bearer " <> apiKey config)

headerRFC3339DatetimeFormat :: Option 'Https
headerRFC3339DatetimeFormat = header "Accept-Datetime-Format" "RFC3339"


asciiLs :: [String]
asciiLs =
  [ "\\x00"
  , "\\x01"
  , "\\x02"
  , "\\x03"
  , "\\x04"
  , "\\x05"
  , "\\x06"
  , "\\x07"
  , "\\x08"
  , "\\t"
  , "\\n"
  , "\\x0b"
  , "\\x0c"
  , "\\r"
  , "\\x0e"
  , "\\x0f"
  , "\\x10"
  , "\\x11"
  , "\\x12"
  , "\\x13"
  , "\\x14"
  , "\\x15"
  , "\\x16"
  , "\\x17"
  , "\\x18"
  , "\\x19"
  , "\\x1a"
  , "\\x1b"
  , "\\x1c"
  , "\\x1d"
  , "\\x1e"
  , "\\x1f"
  , " "
  , "!"
  , "\""
  , "#"
  , "$"
  , "%"
  , "&"
  , "'"
  , "("
  , ")"
  , "*"
  , "+"
  , ","
  , "-"
  , "."
  , "/"
  , "0"
  , "1"
  , "2"
  , "3"
  , "4"
  , "5"
  , "6"
  , "7"
  , "8"
  , "9"
  , ":"
  , ";"
  , "<"
  , "="
  , ">"
  , "?"
  , "@"
  , "A"
  , "B"
  , "C"
  , "D"
  , "E"
  , "F"
  , "G"
  , "H"
  , "I"
  , "J"
  , "K"
  , "L"
  , "M"
  , "N"
  , "O"
  , "P"
  , "Q"
  , "R"
  , "S"
  , "T"
  , "U"
  , "V"
  , "W"
  , "X"
  , "Y"
  , "Z"
  , "["
  , "\\"
  , "]"
  , "^"
  , "_"
  , "`"
  , "a"
  , "b"
  , "c"
  , "d"
  , "e"
  , "f"
  , "g"
  , "h"
  , "i"
  , "j"
  , "k"
  , "l"
  , "m"
  , "n"
  , "o"
  , "p"
  , "q"
  , "r"
  , "s"
  , "t"
  , "u"
  , "v"
  , "w"
  , "x"
  , "y"
  , "z"
  , "{"
  , "|"
  , "}"
  , "~"
  , "\\x7f"
  ]
