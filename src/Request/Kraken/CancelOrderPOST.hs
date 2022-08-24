{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeFamilies          #-}

module Request.Kraken.CancelOrderPOST
  ( CancelOrder(..)
  , CancelOrderConfig (..)
  ) where

import           ApiMaker
import           Control.DeepSeq
import           Data.Aeson
import qualified Data.Text                  as T
import           GHC.Generics

import           Data.Kraken.OrderCancelled
import           Data.Kraken.RequestResult
import           Request.Kraken.Class

-- | Cancel all open orders. Returns the number of orders cancelled.
newtype CancelOrder = CancelOrder CancelOrderConfig

data CancelOrderConfig =
  CancelOrderConfig
    { txid :: T.Text
    }
  deriving (Show, Eq, Ord, ToJSON, Generic, NFData)


instance Request KrakenConfig CancelOrder where
  type Method CancelOrder = POST
  type Body CancelOrder = ReqBodyJson CancelOrderConfig
  type Response CancelOrder = JsonResponse (RequestResult OrderCancelled)
  type Output CancelOrder = OrderCancelled
  method _ CancelOrder {} = POST
  url cfg CancelOrder {} = baseUrl cfg /: "private" /: "CancelOrder"
  body _ (CancelOrder cfg) = ReqBodyJson cfg
  response _ CancelOrder {} = jsonResponse
  requestModifier = addNonceAndApiSign
  option _ CancelOrder{} = return headerRFC3339DatetimeFormat
  process _ CancelOrder{} resp = fromRequestResult (responseBody resp)
