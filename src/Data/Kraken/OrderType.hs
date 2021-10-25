{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
module Data.Kraken.OrderType
    ( OrderType (..)
    , prettyOrderType
    ) where


import           Control.DeepSeq
import           Data.Aeson
import           Data.Serialize
import           Data.Serialize.Text ()
import qualified Data.Text           as T
import           GHC.Generics
import           Text.PrettyPrint

import           Data.Kraken.Util

data OrderType
  = Buy
  | Sell
  deriving (Read, Show, Eq, Ord, Generic, NFData, Serialize)

instance ToJSON OrderType where
  toJSON = genericToJSON jsonSnakeCase

instance FromJSON OrderType where
  parseJSON =
    withText "Data.Kraken.OrderType" $ \s ->
      case T.toLower s of
        "buy"  -> return Buy
        "b"    -> return Buy
        "sell" -> return Sell
        "s"    -> return Sell
        _      -> fail $ "unexpected string in parseJSON in Data.Kraken.OrderType: " ++ show s


prettyOrderType :: OrderType -> Doc
prettyOrderType = text . show
