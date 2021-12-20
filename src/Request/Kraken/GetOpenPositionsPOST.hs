{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeFamilies          #-}


module Request.Kraken.GetOpenPositionsPOST
  ( GetOpenPositions(..)
  , OpenPositionsConfig (..)
  ) where

import           ApiMaker
import           Data.Aeson
import           Data.Char                 (toLower)
import qualified Data.Text                 as T
import           GHC.Generics

import           Data.Kraken.PositionList
import           Data.Kraken.RequestResult
import           Request.Kraken.Class


newtype GetOpenPositions = GetOpenPositions OpenPositionsConfig

data OpenPositionsConfig =
  OpenPositionsConfig
    { opPosCfgTxid          :: Maybe T.Text -- ^ Comma delimited list of txids to limit output to
    , opPosCfgDocalcs       :: Bool         -- ^ Default: False. Whether to include P&L calculations.
    , opPosCfgConsolidation :: Maybe T.Text -- ^ Value: "market". Consolidate positions by market/pair.
    }
  deriving (Show, Read, Eq, Ord, Generic)

instance ToJSON OpenPositionsConfig where
  toJSON = genericToJSON defaultOptions {fieldLabelModifier = map toLower . drop 8, omitNothingFields = True}


instance Request KrakenConfig GetOpenPositions where
  type Method GetOpenPositions = POST
  type Body GetOpenPositions = ReqBodyJson OpenPositionsConfig
  type Response GetOpenPositions = JsonResponse (RequestResult PositionList)
  type Output GetOpenPositions = PositionList
  method _ GetOpenPositions {} = POST
  url cfg GetOpenPositions {} = baseUrl cfg /: "private" /: "OpenPositions"
  body _ (GetOpenPositions config) = ReqBodyJson config
  response _ GetOpenPositions {} = jsonResponse
  requestModifier = addNonceAndApiSign
  option _ GetOpenPositions{} = return headerRFC3339DatetimeFormat
  process _ GetOpenPositions{} resp = fromRequestResult (responseBody resp)
