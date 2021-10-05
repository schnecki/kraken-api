{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeFamilies          #-}


module Request.Kraken.AssetInfoGET
  ( GetAssetInfo(..)
  ) where

import           ApiMaker

import           Data.Kraken.AssetClass
import           Data.Kraken.AssetInfo
import           Data.Kraken.AssetInfoList
import           Data.Kraken.RequestResult
import           Data.Kraken.Types
import           Request.Kraken.Class
import           Request.Kraken.Util


data GetAssetInfo =
  GetAssetInfo InstrumentName (Maybe AssetClass) -- ^ Asset class. (optional, default: currency). Example: aclass=currency


instance Request KrakenConfig GetAssetInfo where
  type Method GetAssetInfo = GET
  type Body GetAssetInfo = NoReqBody
  type Response GetAssetInfo = JsonResponse (RequestResult AssetInfoList)
  type Output GetAssetInfo = AssetInfoList
  method _ GetAssetInfo {} = GET
  url cfg GetAssetInfo {} = baseUrl cfg /: "public" /: "Assets"
  body _ GetAssetInfo {} = NoReqBody
  response _ GetAssetInfo {} = jsonResponse
  option _ GetAssetInfo {} = headerRFC3339DatetimeFormat
  option _ (GetAssetInfo _ mAclass) = headerRFC3339DatetimeFormat <> configs
    where
      configs = "aclass" `queryParam` fmap (strToLower . show) mAclass
  process _ GetAssetInfo {} resp = fromRequestResult $ responseBody resp

