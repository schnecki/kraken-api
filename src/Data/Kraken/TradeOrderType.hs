{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
module Data.Kraken.TradeOrderType
    ( TradeOrderType (..)
    , prettyTradeOrderType
    ) where


import           Control.DeepSeq
import           Data.Aeson
import           Data.Serialize
import           Data.Serialize.Text ()
import qualified Data.Text           as T
import           GHC.Generics
import           Text.PrettyPrint

import           Data.Kraken.Util

data TradeOrderType
  = Market
  | Limit
  deriving (Read, Show, Eq, Ord, Generic, NFData, Serialize)

instance ToJSON TradeOrderType where
  toJSON = genericToJSON jsonSnakeCase

instance FromJSON TradeOrderType where
  parseJSON =
    withText "Data.Kraken.TradeOrderType" $ \s ->
      case T.toLower s of
        "market" -> return Market
        "m"      -> return Market
        "limit"  -> return Limit
        "l"      -> return Limit
        _        -> fail $ "unexpected string in parseJSON in Data.Kraken.TradeOrderType: " ++ show s


prettyTradeOrderType :: TradeOrderType -> Doc
prettyTradeOrderType = text . show
