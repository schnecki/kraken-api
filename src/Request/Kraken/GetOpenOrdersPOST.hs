{-# LANGUAGE DeriveAnyClass        #-}
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
import           Control.Applicative       ((<|>))
import           Control.DeepSeq
import           Data.Aeson
import           Data.Maybe                (fromMaybe)
import           GHC.Generics

import           Data.Kraken.OpenOrderList
import           Data.Kraken.RequestResult
import           Data.Kraken.Types
import           Request.Kraken.Class


newtype GetOpenOrders = GetOpenOrders OpenOrdersConfig

data OpenOrdersConfig = OpenOrdersConfig
  { trades  :: Maybe Bool      -- ^  Whether or not to include trades related to position in output. Default: false.
  , userref :: Maybe Int -- ^ Restrict results to given user reference id
  } deriving (Show, Read, Eq, Ord, Generic, ToJSON)


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
