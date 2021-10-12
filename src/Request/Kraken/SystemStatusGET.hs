{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeFamilies          #-}


module Request.Kraken.SystemStatusGET
  ( GetSystemStatus(..)
  ) where

import           ApiMaker

import           Data.Kraken.RequestResult
import           Data.Kraken.SystemStatus
import           Request.Kraken.Class


data GetSystemStatus = GetSystemStatus

instance Request KrakenConfig GetSystemStatus where
  type Method GetSystemStatus = GET
  type Body GetSystemStatus = NoReqBody
  type Response GetSystemStatus = JsonResponse (RequestResult SystemStatus)
  type Output GetSystemStatus = SystemStatus
  method _ GetSystemStatus {} = GET
  url cfg GetSystemStatus = baseUrl cfg /: "public" /: "SystemStatus"
  body _ GetSystemStatus {} = NoReqBody
  response _ GetSystemStatus {} = jsonResponse
  option _ GetSystemStatus {} = return headerRFC3339DatetimeFormat
  process _ GetSystemStatus {} resp = fromRequestResult $ responseBody resp
