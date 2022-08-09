{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeFamilies          #-}
module Request.Kraken.GetTradesHistoryPOST
  ( GetTradesHistory(..)
  , TradesHistoryConfig (..)
  , HistoryType(..)
  ) where

import           ApiMaker
import           Data.Aeson
import           Data.Char                    (toLower)
import qualified Data.Text                    as T
import           GHC.Generics

import           Data.Kraken.RequestResult
import           Data.Kraken.TradeHistoryList
import           Request.Kraken.Class

-- | Fetches trades.
data GetTradesHistory =
  GetTradesHistory TradesHistoryConfig

data HistoryType = All | AnyPosition | ClosedPosition | ClosingPosition | NoPosition
  deriving (Read, Eq, Ord, Generic)

instance Show HistoryType where
  show All             = "all"
  show AnyPosition     = "any position"
  show ClosedPosition  = "closed position"
  show ClosingPosition = "closing position"
  show NoPosition      = "no position"

instance ToJSON HistoryType where
  toJSON = String . T.pack . show


data TradesHistoryConfig =
  TradesHistoryConfig
    { tradesHistoryType   :: Maybe HistoryType -- ^ Type of trade. Default: All
    , tradesHistoryTrades :: Maybe Bool        -- ^ Whether or not to include trades related to position in output. Default: False
    , tradesHistoryStart  :: Maybe Integer     -- ^ Starting unix timestamp or trade tx ID of results (exclusive).
    , tradesHistoryEnd    :: Maybe Integer     -- ^ Ending unix timestamp or trade tx ID of results (inclusive).
    , tradesHistoryOfs    :: Maybe Integer     -- ^ Result offset for pagination
    }
  deriving (Show, Read, Eq, Ord, Generic)

instance ToJSON TradesHistoryConfig where
  toJSON = genericToJSON defaultOptions {fieldLabelModifier = map toLower . drop 13, omitNothingFields = True}


instance Request KrakenConfig GetTradesHistory where
  type Method GetTradesHistory = POST
  type Body GetTradesHistory = ReqBodyJson TradesHistoryConfig
  type Response GetTradesHistory = JsonResponse (RequestResult TradeHistoryList)
  type Output GetTradesHistory = TradeHistoryList
  method _ GetTradesHistory {} = POST
  url cfg GetTradesHistory {} = baseUrl cfg /: "private" /: "TradesHistory"
  body _ (GetTradesHistory config) = ReqBodyJson config
  requestModifier = addNonceAndApiSign
  response _ GetTradesHistory {} = jsonResponse
  option _ GetTradesHistory{} = return headerRFC3339DatetimeFormat
  process _ _ resp = fromRequestResult (responseBody resp)
