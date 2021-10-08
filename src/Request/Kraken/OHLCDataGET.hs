{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeFamilies          #-}


module Request.Kraken.OHLCDataGET
  ( GetOHLCData(..)
  , OHLCDataConfig (..)
  ) where

import           ApiMaker
import           Data.Maybe                         (fromMaybe)


import           Data.Kraken.CandlestickGranularity
import           Data.Kraken.DateTime
import           Data.Kraken.RequestResult
import           Data.Kraken.TickDataList
import           Data.Kraken.Types
import           Request.Kraken.Class

-- | Fetches up to the last 720 candles. Note: the last entry in the OHLC array is for the current, not-yet-committed frame and will always be present, regardless of the value of since.
--   See also: https://support.kraken.com/hc/en-us/articles/218198197
data GetOHLCData =
  GetOHLCData OHLCDataConfig


data OHLCDataConfig = OHLCDataConfig
  { ohlcDataPair     :: PairName                     -- ^ Example: pair=XXBTCZUSD,XETHXXBT
  , ohlcDataInterval :: Maybe CandlestickGranularity -- ^ Time frame interval in minutes. Example: interval=60.
  , ohlcDataSince    :: Maybe DateTime               -- ^ Return committed OHLC data since given ID. Example: Since=1548111600.
  } deriving (Show, Read, Eq, Ord)


instance Request KrakenConfig GetOHLCData where
  type Method GetOHLCData = GET
  type Body GetOHLCData = NoReqBody
  type Response GetOHLCData = JsonResponse (RequestResult TickDataList)
  type Output GetOHLCData = TickDataList
  method _ GetOHLCData {} = GET
  url cfg GetOHLCData {} = baseUrl cfg /: "public" /: "OHLC"
  body _ GetOHLCData {} = NoReqBody
  response _ GetOHLCData {} = jsonResponse
  option _ (GetOHLCData (OHLCDataConfig p mInt mSince)) = headerRFC3339DatetimeFormat <> configs
    where
      configs =
        "pair"  =:                   p <>
        "interval" `maybeQueryParam` fmap (show . candlestickGranularityInMinutes) mInt <>
        "since"    `maybeQueryParam` fmap dateTimeToPOSIXTime mSince
  process _ (GetOHLCData cfg) resp = setTickDataListGranularity (fromMaybe M1 $ ohlcDataInterval cfg) <$> fromRequestResult (responseBody resp)


