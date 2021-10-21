{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeFamilies          #-}


module Request.Kraken.GetAccountBalancePOST
  ( GetAccountBalance(..)
  ) where

import           ApiMaker

import           Data.Kraken.AccountBalanceList
import           Data.Kraken.RequestResult
import           Request.Kraken.Class


data GetAccountBalance = GetAccountBalance


instance Request KrakenConfig GetAccountBalance where
  type Method GetAccountBalance = POST
  type Body GetAccountBalance = NoReqBody
  type Response GetAccountBalance = JsonResponse (RequestResult AccountBalanceList)
  type Output GetAccountBalance = AccountBalanceList
  method _ GetAccountBalance {} = POST
  url cfg GetAccountBalance {} = baseUrl cfg /: "private" /: "Balance"
  body _ GetAccountBalance {} = NoReqBody
  response _ GetAccountBalance {} = jsonResponse
  requestModifier = addNonceAndApiSign
  option _ GetAccountBalance {} = return headerRFC3339DatetimeFormat
  process _ GetAccountBalance {} resp = fromRequestResult $ responseBody resp
