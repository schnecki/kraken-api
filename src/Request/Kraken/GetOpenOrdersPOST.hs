{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeFamilies          #-}


module Request.Kraken.GetOpenOrdersPOST
  ( GetOpenOrders(..)
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


newtype GetOpenOrders = GetOpenOrders
                          (Maybe InstrumentName) -- ^Base asset used to determine balance. Default: "ZUSD"

newtype OpenOrdersPOST = OpenOrdersPOST
  { asset :: Maybe InstrumentName
  } deriving (Show, Eq, Ord, ToJSON, Generic, NFData)


instance Request KrakenConfig GetOpenOrders where
  type Method GetOpenOrders = POST
  type Body GetOpenOrders = ReqBodyJson OpenOrdersPOST
  type Response GetOpenOrders = JsonResponse (RequestResult OpenOrderList)
  type Output GetOpenOrders = OpenOrderList
  method _ GetOpenOrders {} = POST
  url cfg GetOpenOrders {} = baseUrl cfg /: "private" /: "OpenOrders"
  body _ (GetOpenOrders mInstr) = ReqBodyJson (OpenOrdersPOST mInstr)
  response _ GetOpenOrders {} = jsonResponse
  requestModifier = addNonceAndApiSign
  option _ GetOpenOrders{} = return headerRFC3339DatetimeFormat
  process _ (GetOpenOrders mInstr) resp = setBaseAsset <$> fromRequestResult (responseBody resp)
    where
      setBaseAsset tb = tb {baseAsset = mInstr <|> Just "ZUSD"}
