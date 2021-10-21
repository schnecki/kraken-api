{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeFamilies          #-}


module Request.Kraken.AccountBalancePOST
  ( PostAccountBalance(..)
  ) where

import           ApiMaker

import           Data.Kraken.AccountBalanceList
import           Data.Kraken.AssetClass
import           Data.Kraken.RequestResult
import           Data.Kraken.Types
import           Request.Kraken.Class
import           Request.Kraken.Util


data PostAccountBalance = PostAccountBalance

data PostAccountBalanceBody = PostAccountBalanceBody
  { todo
  }


instance Request KrakenConfig PostAccountBalance where
  type Method PostAccountBalance = POST
  type Body PostAccountBalance = NoReqBody
  type Response PostAccountBalance = JsonResponse (RequestResult AccountBalanceList)
  type Output PostAccountBalance = AccountBalanceList
  method _ PostAccountBalance {} = POST
  url cfg PostAccountBalance {} = baseUrl cfg /: "private" /: "Balance"
  body _ PostAccountBalance {} = NoReqBody
  response _ PostAccountBalance {} = jsonResponse
  requestModifier = mkApiSignature
  option cfg PostAccountBalance {} = withApiSignNonce cfg mempty -- headerRFC3339DatetimeFormat
  process _ PostAccountBalance {} resp = fromRequestResult $ responseBody resp
