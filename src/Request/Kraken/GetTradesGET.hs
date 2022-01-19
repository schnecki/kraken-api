{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeFamilies          #-}
module Request.Kraken.GetTradesGET
  ( GetTrades(..)
  , TradesConfig (..)
  ) where

import           ApiMaker

import           Data.Kraken.DateTime
import           Data.Kraken.RequestResult
import           Data.Kraken.TradeList
import           Data.Kraken.Types
import           Request.Kraken.Class

-- | Fetches up to the last 720 candles. Note: the last entry in the OHLC array is for the current, not-yet-committed frame and will always be present, regardless of the value of since.
--   See also: https://support.kraken.com/hc/en-us/articles/218198197
data GetTrades =
  GetTrades TradesConfig


data TradesConfig = TradesConfig
  { tradesPair  :: PairName                     -- ^ Example: pair=XXBTCZUSD,XETHXXBT
  , tradesSince :: Maybe Integer                -- ^ Return committed Trades data since given ID. Example: Since=1548111600.  The since value is a UNIX timestamp at nanosecond resolution (a standard UNIX timestamp in seconds with 9 additional digits).
  } deriving (Show, Read, Eq, Ord)


instance Request KrakenConfig GetTrades where
  type Method GetTrades = GET
  type Body GetTrades = NoReqBody
  type Response GetTrades = JsonResponse (RequestResult TradeList)
  type Output GetTrades = TradeList
  method _ GetTrades {} = GET
  url cfg GetTrades {} = baseUrl cfg /: "public" /: "Trades"
  body _ GetTrades {} = NoReqBody
  response _ GetTrades {} = jsonResponse
  option _ (GetTrades (TradesConfig p mSince)) = return $ headerRFC3339DatetimeFormat <> configs
    where
      configs =
        "pair"  =:                p <>
        "since"    `maybeQueryParam` fmap show mSince
  process _ _ resp = fromRequestResult (responseBody resp)


