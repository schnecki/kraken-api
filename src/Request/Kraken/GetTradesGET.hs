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
import           Data.Kraken.Trade         (time)
import           Data.Kraken.TradeList
import           Data.Kraken.TradeObject
import           Data.Kraken.Types
import           Request.Kraken.Class

import           Text.Printf

-- | Fetches trades.
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
      configs = "pair" =: p <> "since" `maybeQueryParam` fmap fmtInteger mSince
      fmtInteger :: Integer -> String
      fmtInteger = printf "%d"
  process _ (GetTrades (TradesConfig _ mSince)) resp = filterSince <$>
    fromRequestResult (responseBody resp) -- Kraken sometimes returns data from very long ago
    where
      filterSince (TradeList lst objs) = maybe (TradeList lst objs) (\since -> TradeList lst (map (filterSinceTrObj since) objs)) mSince
      filterSinceTrObj since (TradeObject instr trds) = TradeObject instr (filter ((>= since) . dateTimeToNanoSeconds . time) trds)
