{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeFamilies          #-}


module Request.Kraken.GetClosedOrdersPOST
  ( GetClosedOrders(..)
  , ClosedOrdersConfig (..)
  , CloseTime (..)
  ) where

import           ApiMaker
import           Data.Aeson
import           Data.Char                   (toLower)
import           GHC.Generics

import           Data.Kraken.ClosedOrderList
import           Data.Kraken.RequestResult
import           Request.Kraken.Class


newtype GetClosedOrders = GetClosedOrders ClosedOrdersConfig

data CloseTime = Both | Open | Close
  deriving (Show, Read, Eq, Ord, Generic, ToJSON)

data ClosedOrdersConfig =
  ClosedOrdersConfig
    { clOrdCfgTrades    :: Maybe Bool      -- ^  Whether or not to include trades related to position in output. Default: false.
    , clOrdCfgUserref   :: Maybe Int       -- ^ Restrict results to given user reference id
    , clOrdCfgStart     :: Maybe Int       -- ^ Starting unix timestamp or order tx ID of results (exclusive)
    , clOrdCfgEnd       :: Maybe Int       -- ^ Ending unix timestamp or order tx ID of results (inclusive)
    , clOrdCfgOfs       :: Maybe Int       -- ^ Result offset for pagination
    , clOrdCfgClosetime :: Maybe CloseTime       -- ^ Which time to use to search. Default: "both"
    }
  deriving (Show, Read, Eq, Ord, Generic)


instance ToJSON ClosedOrdersConfig where
  toJSON = genericToJSON defaultOptions {fieldLabelModifier = map toLower . drop 8, omitNothingFields = True}

instance Request KrakenConfig GetClosedOrders where
  type Method GetClosedOrders = POST
  type Body GetClosedOrders = ReqBodyJson ClosedOrdersConfig
  type Response GetClosedOrders = JsonResponse (RequestResult ClosedOrderList)
  type Output GetClosedOrders = ClosedOrderList
  method _ GetClosedOrders {} = POST
  url cfg GetClosedOrders {} = baseUrl cfg /: "private" /: "ClosedOrders"
  body _ (GetClosedOrders config) = ReqBodyJson config
  response _ GetClosedOrders {} = jsonResponse
  requestModifier = addNonceAndApiSign
  option _ GetClosedOrders{} = return headerRFC3339DatetimeFormat
  process _ GetClosedOrders{} resp = fromRequestResult (responseBody resp)
