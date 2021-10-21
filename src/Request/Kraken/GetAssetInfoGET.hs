{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeFamilies          #-}


module Request.Kraken.GetAssetInfoGET
  ( GetAssetInfo(..)
  ) where

import           ApiMaker

import           Data.Kraken.AssetClass
import           Data.Kraken.AssetInfoList
import           Data.Kraken.RequestResult
import           Data.Kraken.Types
import           Request.Kraken.Class
import           Request.Kraken.Util


data GetAssetInfo =
  GetAssetInfo InstrumentName -- ^ Instrument Name. Example: "ADA"
  (Maybe AssetClass)          -- ^ Asset class. (optional, default: currency). Example: Currency


instance Request KrakenConfig GetAssetInfo where
  type Method GetAssetInfo = GET
  type Body GetAssetInfo = NoReqBody
  type Response GetAssetInfo = JsonResponse (RequestResult AssetInfoList)
  type Output GetAssetInfo = AssetInfoList
  method _ GetAssetInfo {} = GET
  url cfg GetAssetInfo {} = baseUrl cfg /: "public" /: "Assets"
  body _ GetAssetInfo {} = NoReqBody
  response _ GetAssetInfo {} = jsonResponse
  option _ (GetAssetInfo instr mAclass) = return $ headerRFC3339DatetimeFormat <> configs
    where
      configs =
        "asset"  =:                instr <>
        "aclass" `maybeQueryParam` fmap (strToLower . show) mAclass
  process _ GetAssetInfo {} resp = fromRequestResult $ responseBody resp


