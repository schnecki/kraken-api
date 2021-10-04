{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}


module Request.SystemStatusGET
  ( GetSystemStatus(..)
  ) where

import           ApiMaker

import           Data.Kraken.RequestResult
import           Data.Kraken.SystemStatus
import           Request.Class


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
  option _ GetSystemStatus {} = headerRFC3339DatetimeFormat
  process _ GetSystemStatus {} resp = fromRequestResult $ responseBody resp
