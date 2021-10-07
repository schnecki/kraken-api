{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeFamilies          #-}


module Request.Kraken.TradableAssetPairsGET
  ( GetTradableAssetPairs(..)
  , TradableAssetPairsConfig (..)
  , TradableAssetPairsInfo (..)
  ) where

import           ApiMaker

import           Data.Kraken.RequestResult
import           Data.Kraken.TradableAssetPairList
import           Data.Kraken.Types
import           Request.Kraken.Class
import           Request.Kraken.Util


data GetTradableAssetPairs =
  GetTradableAssetPairs TradableAssetPairsConfig


data TradableAssetPairsConfig = TradableAssetPairsConfig
  { tradableAssetPair     :: PairName                     -- ^ Example: pair=XXBTCZUSD,XETHXXBT
  , tradableAssetPairInfo :: Maybe TradableAssetPairsInfo -- ^ Enum: "info" "leverage" "fees" "margin". Info to retrieve. (optional). Default: "info"
  } deriving (Show, Read, Eq, Ord)


data TradableAssetPairsInfo
  = Info     -- ^  all info
  | Leverage -- ^  leverage info
  | Fees     -- ^  fees schedule
  | Margin   -- ^  margin info
  deriving (Show, Read, Eq, Ord, Enum, Bounded)


instance Request KrakenConfig GetTradableAssetPairs where
  type Method GetTradableAssetPairs = GET
  type Body GetTradableAssetPairs = NoReqBody
  type Response GetTradableAssetPairs = JsonResponse (RequestResult TradableAssetPairList)
  type Output GetTradableAssetPairs = TradableAssetPairList
  method _ GetTradableAssetPairs {} = GET
  url cfg GetTradableAssetPairs {} = baseUrl cfg /: "public" /: "AssetPairs"
  body _ GetTradableAssetPairs {} = NoReqBody
  response _ GetTradableAssetPairs {} = jsonResponse
  option _ (GetTradableAssetPairs (TradableAssetPairsConfig p i)) = headerRFC3339DatetimeFormat <> configs
    where
      configs =
        "pair"  =:               p <>
        "info" `maybeQueryParam` fmap (strToLower . show) i
  process _ GetTradableAssetPairs {} resp = fromRequestResult $ responseBody resp


