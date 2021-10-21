{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeFamilies          #-}


module Request.Kraken.GetTradeBalancePOST
  ( GetTradeBalance(..)
  ) where

import           ApiMaker
import           Control.Applicative       ((<|>))
import           Control.DeepSeq
import           Data.Aeson
import           Data.Maybe                (fromMaybe)
import           GHC.Generics

import           Data.Kraken.RequestResult
import           Data.Kraken.TradeBalance
import           Data.Kraken.Types
import           Request.Kraken.Class


newtype GetTradeBalance = GetTradeBalance
                          (Maybe InstrumentName) -- ^Base asset used to determine balance. Default: "ZUSD"

newtype TradeBalancePOST = TradeBalancePOST
  { asset :: Maybe InstrumentName
  } deriving (Show, Eq, Ord, ToJSON, Generic, NFData)


instance Request KrakenConfig GetTradeBalance where
  type Method GetTradeBalance = POST
  type Body GetTradeBalance = ReqBodyJson TradeBalancePOST
  type Response GetTradeBalance = JsonResponse (RequestResult TradeBalance)
  type Output GetTradeBalance = TradeBalance
  method _ GetTradeBalance {} = POST
  url cfg GetTradeBalance {} = baseUrl cfg /: "private" /: "TradeBalance"
  body _ (GetTradeBalance mInstr) = ReqBodyJson (TradeBalancePOST mInstr)
  response _ GetTradeBalance {} = jsonResponse
  requestModifier = addNonceAndApiSign
  option _ GetTradeBalance{} = return headerRFC3339DatetimeFormat
  process _ (GetTradeBalance mInstr) resp = setBaseAsset <$> fromRequestResult (responseBody resp)
    where
      setBaseAsset tb = tb {baseAsset = mInstr <|> Just "ZUSD"}
