{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeFamilies          #-}


module Request.Kraken.TickerInformationGET
  ( GetTickerInformation(..)
  ) where

import           ApiMaker

import           Data.Kraken.RequestResult
import           Data.Kraken.TickerInformationList
import           Data.Kraken.Types
import           Request.Kraken.Class


data GetTickerInformation =
  GetTickerInformation PairName -- ^ Pair name. Example: "ADAEUR"

instance Request KrakenConfig GetTickerInformation where
  type Method GetTickerInformation = GET
  type Body GetTickerInformation = NoReqBody
  type Response GetTickerInformation = JsonResponse (RequestResult TickerInformationList)
  type Output GetTickerInformation = TickerInformationList
  method _ GetTickerInformation {} = GET
  url cfg GetTickerInformation {} = baseUrl cfg /: "public" /: "Ticker"
  body _ GetTickerInformation {} = NoReqBody
  response _ GetTickerInformation {} = jsonResponse
  option _ (GetTickerInformation pair) = headerRFC3339DatetimeFormat <> configs
    where
      configs = "pair" =: pair
  process _ GetTickerInformation {} resp = fromRequestResult $ responseBody resp
