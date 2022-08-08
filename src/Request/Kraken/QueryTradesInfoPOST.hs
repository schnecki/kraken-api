{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeFamilies          #-}
module Request.Kraken.QueryTradesInfoPOST
  ( QueryTradesInfo (..)
  , QueryTradesInfoConfig (..)
  ) where

import           ApiMaker
import           Data.Aeson
import           Data.Char                 (toLower)
import qualified Data.Text                 as T
import           GHC.Generics
import           Request.Kraken.Class

import           Data.Kraken.RequestResult
import           Data.Kraken.TradeInfoList

-- ^ Retrieve information about specific trades/fills.
newtype QueryTradesInfo = QueryTradesInfo QueryTradesInfoConfig


data QueryTradesInfoConfig = QueryTradesInfoConfig
  { queryTradesInfotxid   :: T.Text -- ^ Comma delimited list of transaction IDs to query info about (20 maximum)
  , queryTradesInfotrades :: Maybe Bool -- ^ Default: false. Whether or not to include trades related to position in output
  } deriving (Show, Read, Eq, Ord, Generic)

instance ToJSON QueryTradesInfoConfig where
  toJSON = genericToJSON defaultOptions {fieldLabelModifier = map toLower . drop 15, omitNothingFields = True}


instance Request KrakenConfig QueryTradesInfo where
  type Method QueryTradesInfo = POST
  type Body QueryTradesInfo = ReqBodyJson QueryTradesInfoConfig
  type Response QueryTradesInfo = JsonResponse (RequestResult TradeInfoList)
  type Output QueryTradesInfo = TradeInfoList
  method _ QueryTradesInfo {} = POST
  url cfg QueryTradesInfo {} = baseUrl cfg /: "private" /: "QueryTrades"
  body _ (QueryTradesInfo config) = ReqBodyJson config
  response _ QueryTradesInfo {} = jsonResponse
  requestModifier = addNonceAndApiSign
  option _ QueryTradesInfo{} = return headerRFC3339DatetimeFormat
  process _ _ resp = fromRequestResult (responseBody resp)
