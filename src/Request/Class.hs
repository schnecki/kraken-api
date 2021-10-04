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
module Request.Class
  ( Request(..)
  , Session(..)
  , KrakenConfig (..)
  -- , krakenConfigPracticeAccount
  , krakenConfigTradeAccount
  -- , baseUrl
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
import qualified Data.ByteString.Char8       as B
import           Data.Proxy
import           Data.Text                   (Text)
import qualified Data.Text                   as T
import qualified Data.Text.Encoding          as E
import qualified Network.HTTP.Client         as C

import           ApiMaker

version :: Text
version = "0"

type AccessToken = B.ByteString

-- krakenConfigPracticeAccount :: AccessToken -> KrakenConfig
-- krakenConfigPracticeAccount = KrakenConfig baseUrlPractice streamUrlPractice

krakenConfigTradeAccount :: AccessToken -> KrakenConfig
krakenConfigTradeAccount = KrakenConfig baseUrlTrade -- streamUrlTrade


data KrakenConfig = KrakenConfig
  { baseUrl     :: Url 'Https
  -- , streamUrl   :: Url 'Https
  , accessToken :: AccessToken
  }


additionalParams :: KrakenConfig -> [Option 'Https]
additionalParams config = [headerBearer config]

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
headerBearer config = header "Authorization" ("Bearer " <> accessToken config)

headerRFC3339DatetimeFormat :: Option 'Https
headerRFC3339DatetimeFormat = header "Accept-Datetime-Format" "RFC3339"

