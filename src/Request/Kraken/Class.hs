{-# LANGUAGE CPP                     #-}
{-# LANGUAGE DataKinds               #-}
{-# LANGUAGE FlexibleContexts        #-}
{-# LANGUAGE FlexibleInstances       #-}
{-# LANGUAGE MultiParamTypeClasses   #-}
{-# LANGUAGE OverloadedStrings       #-}
{-# LANGUAGE PackageImports          #-}
{-# LANGUAGE RankNTypes              #-}
{-# LANGUAGE TemplateHaskell         #-}
{-# LANGUAGE TypeFamilies            #-}
{-# LANGUAGE UndecidableInstances    #-}
{-# LANGUAGE UndecidableSuperClasses #-}
module Request.Kraken.Class
  ( Request(..)
  , Session(..)
  , KrakenConfig (..)
  -- , krakenConfigPracticeAccount
  , krakenConfigTradeAccount
  -- , baseUrl
  -- , withApiSignNonce
  , addNonceAndApiSign
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
import qualified Crypto.Hash.SHA256                         as SHA256
import qualified Crypto.Hash.SHA512                         as SHA512
import           Data.Aeson
import           Data.Aeson.Key
import qualified Data.Aeson.KeyMap                          as HM
import           Data.Bifunctor                             (bimap)
import qualified Data.ByteString                            as B
import qualified Data.ByteString                            as BS
import           Data.ByteString.Builder
import qualified Data.ByteString.Internal                   as B
import           Data.Char
import           Data.List                                  (foldl')
import           Data.Maybe                                 (fromMaybe)
import           Data.Text.Internal.Unsafe.Char
import qualified Data.Word                                  as W
import           EasyLogger
import           GHC.Exts
import           GHC.Show                                   (showLitChar)
import           Numeric                                    (showHex)
import           Text.Printf
-- import qualified Data.ByteString.Lazy        as BS
import qualified Data.Binary.Builder                        as BinB
import qualified Data.ByteString.Base16                     as B16
import qualified "base64-bytestring" Data.ByteString.Base64 as B64
import qualified Data.ByteString.Lazy.Char8                 as BL
import qualified Data.CaseInsensitive                       as CI
import           Data.Kraken.Util
import           Data.List                                  (find)
import           Data.Maybe                                 (maybe)
import           Data.Proxy
import           Data.Text                                  (Text)
import qualified Data.Text                                  as T
import qualified Data.Text.Encoding                         as E
import           Data.Time.Clock.POSIX                      (getPOSIXTime)
import           EasyLogger
import           Network.HTTP.Base                          (urlEncodeVars)
import qualified Network.HTTP.Client                        as C
import qualified Network.HTTP.Types.Header                  as C (RequestHeaders)

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
  KrakenConfig baseUrlTrade apiKey (B64.decodeLenient prApiKey) mOtp

data KrakenConfig =
  KrakenConfig
    { baseUrl           :: Url 'Https
    , apiKey            :: APIKey
    , privateKeyDecoded :: B.ByteString
    , mOtp              :: Maybe OTP
    }


additionalParams :: KrakenConfig -> [Option 'Https]
additionalParams config = [] -- header "Content-Type" "application/x-www-form-urlencoded; charset=utf-8"]


-- | Creates the required bodies and headers for sending requests to the private API.
addNonceAndApiSign :: KrakenConfig -> r -> C.Request -> IO C.Request
addNonceAndApiSign cfg _ r = do
  nonce <- mkNonce
  res@(headers, body) <- mkSignAndHeaders cfg r . E.encodeUtf8 . T.pack $ show nonce
  $(logPrintDebugText) $ "Nonce: " <> T.pack (show nonce)
  $(logPrintDebugText) $ "Headers: " <> T.pack (show headers)
  $(logPrintDebugText) $ "Body: " <> T.pack (show body)
  setBodyAndHeader res
  where
    setBodyAndHeader (headers, bodies) = return $ r {C.requestHeaders = headers, C.requestBody = C.RequestBodyBS bodies}
    mkNonce :: IO Integer
    mkNonce = round . (* 1000) <$> getPOSIXTime


-- | Make Nonce and Hash for API Sign. Returns the header and body to be set (including the already set values).
--
--   HMAC-SHA512 of (URI path + SHA256(nonce + POST data)) and base64 decoded secret API key
--
mkSignAndHeaders :: KrakenConfig -> C.Request -> BS.ByteString -> IO (C.RequestHeaders, B.ByteString)
mkSignAndHeaders cfg r nonce = return (requestHeaders, requestBodyBs)
  where
    otpHeader = maybe [] (\otp -> [(CI.mk $ toBS "otp", otp)]) (mOtp cfg)
    requestHeaders = (CI.mk $ toBS "API-Key", apiKey cfg) : otpHeader ++ [(CI.mk $ toBS "API-Sign", hash)]
                     -- ++ C.requestHeaders r -- Kraken does not accept the Nonce if there are more HEADERs!
    hash = B64.encode $ SHA512.hmac (privateKeyDecoded cfg) message
    message = C.path r <> sha256Data
    sha256Data = SHA256.hash (nonce <> requestBodyBs)
    bodyData = ("nonce", Just nonce) : bodyToBS (C.requestBody r)
    requestBodyBs = B.intercalate "&" $ map (\(k, v) -> k <> "=" <> fromMaybe "" v) bodyData
    toBS = E.encodeUtf8 . T.pack


-- | Convert @RequestBody@ to key-value pairs.
bodyToBS :: C.RequestBody -> [(B.ByteString, Maybe B.ByteString)]
bodyToBS (C.RequestBodyLBS x)       = parseBSList x
bodyToBS (C.RequestBodyBS x)        = parseBSList $ BL.fromStrict x
bodyToBS (C.RequestBodyBuilder _ x) = parseBSList $ BinB.toLazyByteString x
bodyToBS _                          = error "Streaming request bodies are not (yet) supported in kraken-api"

data Obj =
  Obj
    { fromObj :: [(B.ByteString, Maybe B.ByteString)]
    }

instance FromJSON Obj where
  parseJSON = withObject "Data.Kraken.Class.Obj" $ \o -> Obj . zip (map (E.encodeUtf8 . toText) . HM.keys $ o) . map (fmap E.encodeUtf8) <$> mapM (parseToStr >=> parseJSON) (HM.elems o)

-- | Parses the arguments ByteString to a list of arguments, in order to generate the API Signature requested by Kraken.
parseBSList :: BL.ByteString -> [(B.ByteString, Maybe B.ByteString)]
parseBSList x
  | BL.null x = []
  | otherwise = fromObj . fromEither . eitherDecode' $ x
  where
    fromEither :: Either String Obj -> Obj
    fromEither (Right v)  = v
    fromEither (Left str) = $(pureLogPrintError) ("Parse error in Request.Kraken.Class.Obj: " <> T.pack str) (Obj [])


baseUrlTrade :: Url 'Https
baseUrlTrade = https "api.kraken.com" /: version

headerRFC3339DatetimeFormat :: Option 'Https
headerRFC3339DatetimeFormat = header "Accept-Datetime-Format" "RFC3339"
