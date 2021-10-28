{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeFamilies          #-}


module Request.Kraken.GetOpenOrdersPOST
  ( GetOpenOrders(..)
  , OpenOrdersConfig (..)
  ) where

import           ApiMaker
import           Data.Aeson
import           Data.Char                 (toLower)
import           GHC.Generics

import           Data.Kraken.OpenOrderList
import           Data.Kraken.RequestResult
import           Request.Kraken.Class


newtype GetOpenOrders = GetOpenOrders OpenOrdersConfig

data OpenOrdersConfig = OpenOrdersConfig
  { opOrdCfgTrades  :: Maybe Bool      -- ^  Whether or not to include trades related to position in output. Default: false.
  , opOrdCfgUserref :: Maybe Int -- ^ Restrict results to given user reference id
  } deriving (Show, Read, Eq, Ord, Generic)

instance ToJSON OpenOrdersConfig where
  toJSON = genericToJSON defaultOptions {fieldLabelModifier = map toLower . drop 8, omitNothingFields = True}


instance Request KrakenConfig GetOpenOrders where
  type Method GetOpenOrders = POST
  type Body GetOpenOrders = ReqBodyJson OpenOrdersConfig
  type Response GetOpenOrders = JsonResponse (RequestResult OpenOrderList)
  type Output GetOpenOrders = OpenOrderList
  method _ GetOpenOrders {} = POST
  url cfg GetOpenOrders {} = baseUrl cfg /: "private" /: "OpenOrders"
  body _ (GetOpenOrders config) = ReqBodyJson config
  response _ GetOpenOrders {} = jsonResponse
  requestModifier = addNonceAndApiSign
  option _ GetOpenOrders{} = return headerRFC3339DatetimeFormat
  process _ GetOpenOrders{} resp = fromRequestResult (responseBody resp)
