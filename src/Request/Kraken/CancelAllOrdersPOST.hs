{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeFamilies          #-}

module Request.Kraken.CancelAllOrdersPOST
  ( CancelAllOrders(..)
  ) where

import           ApiMaker
import           Data.Aeson
import           GHC.Generics

import           Data.Kraken.RequestResult
import           Request.Kraken.Class


-- | Cancel all open orders. Returns the number of orders cancelled.
data CancelAllOrders = CancelAllOrders

data CancelAllOrdersResult =
  CancelAllOrdersResult
    { count :: Int
    }
  deriving (Generic, FromJSON)

instance Request KrakenConfig CancelAllOrders where
  type Method CancelAllOrders = POST
  type Body CancelAllOrders = NoReqBody
  type Response CancelAllOrders = JsonResponse (RequestResult CancelAllOrdersResult)
  type Output CancelAllOrders = Int
  method _ CancelAllOrders {} = POST
  url cfg CancelAllOrders {} = baseUrl cfg /: "private" /: "CancelAll"
  body _ CancelAllOrders = NoReqBody
  response _ CancelAllOrders {} = jsonResponse
  requestModifier = addNonceAndApiSign
  option _ CancelAllOrders{} = return headerRFC3339DatetimeFormat
  process _ CancelAllOrders{} resp = count <$> fromRequestResult (responseBody resp)
