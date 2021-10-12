{-# LANGUAGE CPP                        #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
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
  , withAPISign
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
import           Data.ByteString.Base64      as B64
import qualified Data.ByteString.Char8       as B
import qualified Data.ByteString.Lazy.Char8  as BL
import qualified Data.CaseInsensitive        as CI
import           Data.Digest.Pure.SHA
import           Data.List                   (find)
import           Data.Maybe                  (maybe)
import           Data.Proxy
import           Data.Text                   (Text)
import qualified Data.Text                   as T
import qualified Data.Text.Encoding          as E
import           Data.Time.Clock.POSIX       (getPOSIXTime)
import qualified Network.HTTP.Client         as C

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
  KrakenConfig baseUrlTrade apiKey (BL.fromStrict $ B64.decodeLenient prApiKey) mOtp


data KrakenConfig =
  KrakenConfig
    { baseUrl           :: Url 'Https
    , apiKey            :: APIKey
    , privateKeyDecoded :: BL.ByteString
    , otp               :: Maybe OTP
    }


additionalParams :: KrakenConfig -> [Option 'Https]
additionalParams config = [headerBearer config]

-- | Sign the API call with the given API-Key, add nonce and OTP if given.
withAPISign :: KrakenConfig -> [Option 'Https] -> IO [Option 'Https]
withAPISign cfg ops = (<> ops') <$> mkNonce
  where ops' = ops <> mkApiKey cfg <> mkOtp cfg

mkApiKey :: KrakenConfig -> [Option 'Https]
mkApiKey cfg = [header "API-Key" (apiKey cfg)]

mkOtp :: KrakenConfig -> [Option 'Https]
mkOtp cfg = maybe mempty (pure . header "otp") (otp cfg)

mkNonce :: IO [Option 'Https]
mkNonce = pure . header "nonce" . E.encodeUtf8 . T.pack <$> unixTimestamp
  where unixTimestamp = show . round . (* 1000) <$> getPOSIXTime

-- HMAC-SHA512 of (URI path + SHA256(nonce + POST data)) and base64 decoded secret API key
mkApiSignature :: KrakenConfig -> r -> C.Request -> C.Request
mkApiSignature cfg _ r = attachHeader "API-Sign" (BL.toStrict $ bytestringDigest hash) r
  where
    hash = hmacSha512 (privateKeyDecoded cfg) apiSign
    headers = C.requestHeaders r
    nNonce = CI.mk "nonce"
    nonce = maybe (error $ "Could not find nonce in request " ++ show r ++ ", but expected a nonce in the headers!") snd $ find ((== nNonce) . fst) headers
    apiSign = uriPath <> bytestringDigest encodedData

    uriPath = BL.fromStrict .E.encodeUtf8 . T.pack $ show (C.path r)
    encodedData = sha256 (BL.fromStrict . E.encodeUtf8 . T.pack $ show nonce ++ tail (show (C.queryString r)))


baseUrlTrade :: Url 'Https
baseUrlTrade = https "api.kraken.com" /: version

-- baseUrlPractice :: Url 'Https
-- baseUrlPractice = https "api.kraken.com" /: version

-- streamUrlTrade :: Url 'Https
-- streamUrlTrade = https "stream-fxtrade.kraken.com" /: version

-- streamUrlPractice :: Url 'Https
-- streamUrlPractice = https "stream-fxpractice.kraken.com" /: version

-- TODO: see http://hdiff.luite.com/cgit/kraken-rest-api/commit?id=0.4.0 for streaming

headerBearer :: KrakenConfig -> Option 'Https
headerBearer config =
  -- header "Authorization" ("Bearer " <> accessToken config)
  header "API-Key" ("Bearer " <> apiKey config) <>
  header "API-Key" ("Bearer " <> apiKey config)

headerRFC3339DatetimeFormat :: Option 'Https
headerRFC3339DatetimeFormat = header "Accept-Datetime-Format" "RFC3339"

