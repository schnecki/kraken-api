{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeFamilies          #-}
module Request.Kraken.RecentSpreadsGET
  ( GetRecentSpreads(..)
  , RecentSpreadsConfig (..)
  ) where

import           ApiMaker

import           Data.Kraken.DateTime
import           Data.Kraken.RequestResult
import           Data.Kraken.SpreadList
import           Data.Kraken.Types
import           Request.Kraken.Class

-- | Fetches up to the last 720 candles. Note: the last entry in the OHLC array is for the current, not-yet-committed frame and will always be present, regardless of the value of since.
--   See also: https://support.kraken.com/hc/en-us/articles/218198197
data GetRecentSpreads =
  GetRecentSpreads RecentSpreadsConfig


data RecentSpreadsConfig = RecentSpreadsConfig
  { spreadsPair  :: PairName                     -- ^ Example: pair=XXBTCZUSD,XETHXXBT
  , spreadsSince :: Maybe Integer                -- ^ Return spread data since given ID
  } deriving (Show, Read, Eq, Ord)


instance Request KrakenConfig GetRecentSpreads where
  type Method GetRecentSpreads = GET
  type Body GetRecentSpreads = NoReqBody
  type Response GetRecentSpreads = JsonResponse (RequestResult SpreadList)
  type Output GetRecentSpreads = SpreadList
  method _ GetRecentSpreads {} = GET
  url cfg GetRecentSpreads {} = baseUrl cfg /: "public" /: "Spread"
  body _ GetRecentSpreads {} = NoReqBody
  response _ GetRecentSpreads {} = jsonResponse
  option _ (GetRecentSpreads (RecentSpreadsConfig p mSince)) = headerRFC3339DatetimeFormat <> configs
    where
      configs =
        "pair"  =:                   p <>
        "since"    `maybeQueryParam` fmap show mSince
  process _ _ resp = fromRequestResult (responseBody resp)


