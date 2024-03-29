{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
module Request.Kraken.AddOrderPOST
  ( AddOrder(..)
  , AddOrderConfig (..)
  , AddOrderType (..)
  , AddOrderTrigger (..)
  , AddOrderSelfTradePrevention(..)
  , OrderFlagList
  ) where

import           ApiMaker
import           Control.DeepSeq
import           Data.Aeson
import           Data.Char                 (toLower)
import           Data.Serialize
import qualified Data.Text                 as T
import           Data.Time
import           Data.Time.RFC3339
import           EasyLogger
import           GHC.Generics

import           Data.Kraken.OrderAdded
import           Data.Kraken.OrderFlags
import           Data.Kraken.OrderType
import           Data.Kraken.PositionList
import           Data.Kraken.RequestResult
import           Data.Kraken.TimeInForce
import           Request.Kraken.Class

import           Data.Kraken.Types


newtype AddOrder = AddOrder AddOrderConfig


data AddOrderType
  = AddOrderMarket
  | AddOrderLimit
  | AddOrderStopLoss
  | AddOrderTakeProfit
  | AddOrderStopLossLimit
  | AddOrderTakeProfitLimit
  | AddOrderSettlePosition
  deriving (Show, Read, Eq, Ord, Generic, Serialize, NFData)

instance ToJSON AddOrderType where
  toJSON AddOrderMarket          = String "market"
  toJSON AddOrderLimit           = String "limit"
  toJSON AddOrderStopLoss        = String "stop-loss"
  toJSON AddOrderTakeProfit      = String "take-profit"
  toJSON AddOrderStopLossLimit   = String "stop-loss-limit"
  toJSON AddOrderTakeProfitLimit = String "take-profit-limit"
  toJSON AddOrderSettlePosition  = String "settle-position"


data AddOrderTrigger
  = AddOrderLast
  | AddOrderIndex
    deriving (Show, Read, Eq, Ord, Generic, Serialize, NFData)

instance ToJSON AddOrderTrigger where
  toJSON AddOrderLast  = String "last"
  toJSON AddOrderIndex = String "index"


data AddOrderSelfTradePrevention
  = AddOrderSTPCancelNewest
  | AddOrderSTPCancelOldest
  | AddOrderSTPCancelBoth
  deriving (Show, Read, Eq, Ord, Generic, Serialize, NFData)

instance ToJSON AddOrderSelfTradePrevention where
  toJSON AddOrderSTPCancelNewest = String "cancel-newest"
  toJSON AddOrderSTPCancelOldest = String "cancel-oldest"
  toJSON AddOrderSTPCancelBoth   = String "cancel-both"

newtype OrderFlagList = OrderFlagList [OrderFlags]
    deriving (Show, Read, Eq, Ord, Generic, Serialize, NFData)

instance ToJSON OrderFlagList where
  toJSON (OrderFlagList xs) = String $ T.intercalate "," $ map (T.pack . show) xs


data AddOrderConfig =
  AddOrderConfig
    { addOrderUserref        :: Maybe Integer         -- ^ User reference id. Userref is an optional user-specified integer id that can be associated with any number of orders. Many clients choose a
                                                      -- userref corresponding to a unique integer id generated by their systems (e.g. a timestamp). However, because we don't enforce uniqueness on our
                                                      -- side, it can also be used to easily group orders by pair, side, strategy, etc. This allows clients to more readily cancel or query information
                                                      -- about orders in a particular group, with fewer API calls by using userref instead of our txid, where supported.
    , addOrderOrderType      :: AddOrderType          -- ^ Order type. Enum: "market" "limit" "stop-loss" "take-profit" "stop-loss-limit" "take-profit-limit" "settle-position"
    , addOrderType           :: OrderType             -- ^ Order direction (buy/sell). Enum: "buy" "sell"
    , addOrderVolume         :: T.Text                -- ^ Order quantity in terms of the base asset. Note: Volume can be specified as 0 for closing margin orders to automatically fill the requisite
                                                      -- quantity.
    , addOrderPair           :: InstrumentName        -- ^ Asset pair id or altname.
    , addOrderPrice          :: Maybe T.Text          -- ^ Price. Limit price for limit orders. Trigger price for stop-loss, stop-loss-limit, take-profit and take-profit-limit orders.
    , addOrderPrice2         :: Maybe T.Text          -- ^ Secondary Price. Limit price for stop-loss-limit and take-profit-limit orders. Note: Either price or price2 can be preceded by +, -, or # to
                                                      -- specify the order price as an offset relative to the last traded price. + adds the amount to, and - subtracts the amount from the last traded
                                                      -- price. # will either add or subtract the amount to the last traded price, depending on the direction and order type used. Relative prices can
                                                      -- be suffixed with a % to signify the relative amount as a percentage.
    , addOrderTrigger        :: Maybe AddOrderTrigger -- ^ Default: "last". Enum: "index" "last". Price signal used to trigger stop-loss, stop-loss-limit, take-profit and take-profit-limit orders.
                                                      -- Note: This trigger type will as well be used for associated conditional close orders.
    , addOrderLeverage       :: Maybe T.Text          -- ^ Amount of leverage desired (default = none)
    , addOrderStp_type       :: Maybe AddOrderSelfTradePrevention -- ^ Default: "cancel-newest". Enum: "cancel-newest" "cancel-oldest" "cancel-both". Self trade prevention behavior definition. cancel-newest - if
                                                      -- self trade is triggered, arriving order will be canceled. cancel-oldest - if self trade is triggered, resting order will be canceled.
                                                      -- cancel-both - if self trade is triggered, both arriving and resting orders will be canceled.
    , addOrderOflags         :: Maybe OrderFlagList    -- ^ Comma delimited list of order flags. "post" post-only order (available when ordertype = limit). "fcib" prefer fee in base currency (default
                                                      -- if selling). "fciq" prefer fee in quote currency (default if buying, mutually exclusive with fcib). "nompp" disable market price protection for
                                                      -- market orders. "viqc" order volume expressed in quote currency. This is supported only for market orders.
    , addOrderTimeInForce    :: Maybe TimeInForce     -- ^ Default: "GTC". Enum: "GTC" "IOC" "GTD". Time-in-force of the order to specify how long it should remain in the order book before being
                                                      -- cancelled. GTC (Good-'til-cancelled) is default if the parameter is omitted. IOC (immediate-or-cancel) will immediately execute the amount
                                                      -- possible and cancel any remaining balance rather than resting in the book. GTD (good-'til-date), if specified, must coincide with a desired
                                                      -- expiretm.
    , addOrderStarttm        :: Maybe T.Text          -- ^ Scheduled start time. Can be specified as an absolute timestamp or as a number of seconds in the future. 0 now (default). +<n> schedule start
                                                      -- time seconds from now. <n> = unix timestamp of start time.
    , addOrderExpiretm       :: Maybe T.Text          -- ^ Expiration time. 0 no expiration (default). +<n> = expire seconds from now, minimum 5 seconds. <n> = unix timestamp of expiration time.
    , addOrderCloseOrderType :: Maybe AddOrderType    -- ^ Conditional close order type. Enum: "limit" "stop-loss" "take-profit" "stop-loss-limit" "take-profit-limit". "market is not allowed"! Note:
                                                      -- Conditional close orders are triggered by execution of the primary order in the same quantity and opposite direction, but once triggered are
                                                      -- independent orders that may reduce or increase net position.
    , addOrderClosePrice     :: Maybe T.Text          -- ^ Conditional close order price.
    , addOrderClosePrice2    :: Maybe T.Text          -- ^ Conditional close order price2
    , addOrderDeadline       :: Maybe T.Text          -- ^ RFC3339 timestamp (e.g. 2021-04-01T00:18:45Z) after which the matching engine should reject the new order request, in presence of latency or
                                                      -- order queueing. min now() + 2 seconds, max now() + 60 seconds.
    , addOrderValidate       :: Maybe Bool            -- ^ Validate inputs only. Do not submit order. Default: false
    }
  deriving (Show, Read, Eq, Ord, Generic)

instance ToJSON AddOrderConfig where
  toJSON =
    -- fixValues $
    genericToJSON defaultOptions {fieldLabelModifier = putBrackets . map toLower . drop 8, omitNothingFields = True}
    where
      -- fixValues (Object m) = Object $ alterF (\mVal -> fmtUTC <$> mVal) "deadline" m
      -- fmtUTC (Object time) = String $ formatTimeRFC3339 (utcToZonedTime utc time)
      putBrackets x =
        case x of
          "closeprice"     -> "close[price]"
          "closeprice2"    -> "close[price2]"
          "closeordertype" -> "close[ordertype]"
          _                -> x


instance Request KrakenConfig AddOrder where
  type Method AddOrder = POST
  type Body AddOrder = ReqBodyJson AddOrderConfig
  type Response AddOrder = JsonResponse (RequestResult OrderAdded)
  type Output AddOrder = OrderAdded
  method _ AddOrder {} = POST
  url cfg AddOrder {} = baseUrl cfg /: "private" /: "AddOrder"
  body _ (AddOrder config) = ReqBodyJson config
  response _ AddOrder {} = jsonResponse
  requestModifier = addNonceAndApiSign
  option _ AddOrder{} = return headerRFC3339DatetimeFormat
  process _ AddOrder{} resp = do
    $(logDebugText) $ "AddOrder response: " <> T.pack (show resp)
    fromRequestResult (responseBody resp)
