{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeFamilies          #-}


module Request.Kraken.OrderBookGET
  ( GetOrderBook(..)
  , OrderBookConfig (..)
  ) where

import           ApiMaker

import           Data.Kraken.OrderBookList
import           Data.Kraken.RequestResult
import           Data.Kraken.Types
import           Request.Kraken.Class

-- | Fetches up to the last 720 candles. Note: the last entry in the OHLC array is for the current, not-yet-committed frame and will always be present, regardless of the value of since.
--   See also: https://support.kraken.com/hc/en-us/articles/218198197
data GetOrderBook =
  GetOrderBook OrderBookConfig


data OrderBookConfig = OrderBookConfig
  { orderBookPair  :: PairName                     -- ^ Example: pair=XXBTCZUSD,XETHXXBT
  , orderBookCount :: Maybe Int                    -- ^ Maximum number of asks/bids. [1..500]. Default: 100. Example: count=2
  } deriving (Show, Read, Eq, Ord)


instance Request KrakenConfig GetOrderBook where
  type Method GetOrderBook = GET
  type Body GetOrderBook = NoReqBody
  type Response GetOrderBook = JsonResponse (RequestResult OrderBookList)
  type Output GetOrderBook = OrderBookList
  method _ GetOrderBook {} = GET
  url cfg GetOrderBook {} = baseUrl cfg /: "public" /: "Depth"
  body _ GetOrderBook {} = NoReqBody
  response _ GetOrderBook {} = jsonResponse
  option _ (GetOrderBook (OrderBookConfig p mInt)) = headerRFC3339DatetimeFormat <> configs
    where
      configs =
        "pair"  =:                p <>
        "count" `maybeQueryParam` fmap show mInt
  process _ _ resp = fromRequestResult (responseBody resp)


