{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeFamilies          #-}


module Request.Kraken.GetServerTimeGET
  ( GetServerTime(..)
  ) where

import           ApiMaker

import           Data.Kraken.RequestResult
import           Data.Kraken.ServerTime
import           Request.Kraken.Class


data GetServerTime = GetServerTime

instance Request KrakenConfig GetServerTime where
  type Method GetServerTime = GET
  type Body GetServerTime = NoReqBody
  type Response GetServerTime = JsonResponse (RequestResult ServerTime)
  type Output GetServerTime = ServerTime
  method _ GetServerTime {} = GET
  url cfg GetServerTime = baseUrl cfg /: "public" /: "Time"
  body _ GetServerTime {} = NoReqBody
  response _ GetServerTime {} = jsonResponse
  option _ GetServerTime {} = return headerRFC3339DatetimeFormat
  process _ GetServerTime {} resp = fromRequestResult $ responseBody resp
