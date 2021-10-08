{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
module Data.Kraken.TradeType
    ( TradeType (..)
    , prettyTradeType
    ) where


import           Control.DeepSeq
import           Data.Aeson
import           Data.Serialize
import           Data.Serialize.Text ()
import qualified Data.Text           as T
import           GHC.Generics
import           Text.PrettyPrint

import           Data.Kraken.Util

data TradeType
  = Buy
  | Sell
  deriving (Read, Show, Eq, Ord, Generic, NFData, Serialize)

instance ToJSON TradeType where
  toJSON = genericToJSON jsonSnakeCase

instance FromJSON TradeType where
  parseJSON =
    withText "Data.Kraken.TradeType" $ \s ->
      case T.toLower s of
        "buy"  -> return Buy
        "b"    -> return Buy
        "sell" -> return Sell
        "s"    -> return Sell
        _      -> fail $ "unexpected string in parseJSON in Data.Kraken.TradeType: " ++ show s


prettyTradeType :: TradeType -> Doc
prettyTradeType = text . show
