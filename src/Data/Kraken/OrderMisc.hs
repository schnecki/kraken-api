{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
module Data.Kraken.OrderMisc
    ( OrderMisc (..)
    , prettyOrderMisc
    ) where

import           Control.DeepSeq
import           Data.Aeson
import           Data.Serialize
import           GHC.Generics
import           Text.PrettyPrint

import           Data.Kraken.Util

data OrderMisc
  = Stopped    -- ^ triggered by stop price
  | Touched    -- ^ triggered by touch price
  | Liquidated -- ^ liquidation
  | Partial    -- ^ partial fill
  deriving (Enum, Bounded, Read, Show, Eq, Ord, Generic, NFData, Serialize)


instance ToJSON OrderMisc where
   toJSON = genericToJSON jsonSnakeCase

instance FromJSON OrderMisc where
  parseJSON = genericParseJSON jsonSnakeCase


prettyOrderMisc :: OrderMisc -> Doc
prettyOrderMisc = text . show
