{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
module Data.Kraken.OrderStatus
    ( OrderStatus (..)
    , PositionStatus
    , prettyOrderStatus
    ) where

import           Control.DeepSeq
import           Data.Aeson
import           Data.Serialize
import           GHC.Generics
import           Text.PrettyPrint

import           Data.Kraken.Util

type PositionStatus = OrderStatus

data OrderStatus
  = Pending  -- ^ order pending book entry
  | Open     -- ^ open order
  | Closed   -- ^ closed order
  | Canceled -- ^ order canceled
  | Expired  -- ^ order expired
  deriving (Enum, Bounded, Read, Show, Eq, Ord, Generic, NFData, Serialize)


instance ToJSON OrderStatus where
   toJSON = genericToJSON jsonSnakeCase

instance FromJSON OrderStatus where
  parseJSON = genericParseJSON jsonSnakeCase


prettyOrderStatus :: OrderStatus -> Doc
prettyOrderStatus = text . show
