{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeFamilies          #-}

-- | POST /0/private/EditOrder
--
-- Edit the parameters of a live unfilled pending order.
-- Cancels the existing order and creates a replacement with new parameters.
-- The new order receives a new @txid@; the original is stored in @originaltxid@.
--
-- NOTE: Applies to PENDING (unfilled) orders only.
-- To partially close a filled margin position use 'AddOrderPOST' with
-- 'AddOrderSettlePosition' order type and the desired close volume.
module Request.Kraken.EditOrderPOST
  ( EditOrder (..)
  , EditOrderConfig (..)
  , EditOrderResult (..)
  ) where

import           ApiMaker
import           Control.DeepSeq
import           Data.Aeson
import qualified Data.Text          as T
import           GHC.Generics

import           Data.Kraken.RequestResult
import           Data.Kraken.Types
import           Request.Kraken.Class


newtype EditOrder = EditOrder EditOrderConfig

-- | Request parameters for EditOrder.
data EditOrderConfig = EditOrderConfig
  { editOrderTxid     :: T.Text         -- ^ Original order ID to edit (required).
  , editOrderPair     :: InstrumentName -- ^ Asset pair (required for non-crypto pairs).
  , editOrderUserref  :: Maybe Integer  -- ^ Optional new user reference id.
  , editOrderVolume   :: Maybe T.Text   -- ^ New order volume in base asset.
  , editOrderPrice    :: Maybe T.Text   -- ^ New limit or trigger price.
  , editOrderPrice2   :: Maybe T.Text   -- ^ New secondary price (stop-loss-limit / take-profit-limit).
  , editOrderValidate :: Maybe Bool     -- ^ Validate only; do not submit order.
  } deriving (Show, Eq, Ord, Generic, NFData)

instance ToJSON EditOrderConfig where
  toJSON cfg = object $ filter (\(_, v) -> v /= Null)
    [ "txid"     .= editOrderTxid     cfg
    , "pair"     .= editOrderPair     cfg
    , "userref"  .= editOrderUserref  cfg
    , "volume"   .= editOrderVolume   cfg
    , "price"    .= editOrderPrice    cfg
    , "price2"   .= editOrderPrice2   cfg
    , "validate" .= editOrderValidate cfg
    ]

-- | Result returned by EditOrder on success.
data EditOrderResult = EditOrderResult
  { editStatus          :: T.Text  -- ^ "ok" on success.
  , editTxid            :: T.Text  -- ^ New order ID assigned by Kraken.
  , editOriginalTxid    :: T.Text  -- ^ ID of the original (now cancelled) order.
  , editVolume          :: T.Text  -- ^ Updated volume.
  , editPrice           :: T.Text  -- ^ Updated price.
  , editOrdersCancelled :: Int     -- ^ Always 1 for a successful edit.
  } deriving (Show, Eq, Generic, NFData)

instance FromJSON EditOrderResult where
  parseJSON = withObject "EditOrderResult" $ \o -> EditOrderResult
    <$> o .: "status"
    <*> o .: "txid"
    <*> o .: "originaltxid"
    <*> o .: "volume"
    <*> o .: "price"
    <*> o .: "orders_cancelled"

instance Request KrakenConfig EditOrder where
  type Method   EditOrder = POST
  type Body     EditOrder = ReqBodyJson EditOrderConfig
  type Response EditOrder = JsonResponse (RequestResult EditOrderResult)
  type Output   EditOrder = EditOrderResult
  method   _ EditOrder {}      = POST
  url      cfg EditOrder {}    = baseUrl cfg /: "private" /: "EditOrder"
  body     _ (EditOrder cfg)   = ReqBodyJson cfg
  response _ EditOrder {}      = jsonResponse
  requestModifier              = addNonceAndApiSign
  option   _ EditOrder {}      = return headerRFC3339DatetimeFormat
  process  _ EditOrder {} resp = fromRequestResult (responseBody resp)
