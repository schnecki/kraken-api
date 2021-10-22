{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
module Data.Kraken.OrderFlags
    ( OrderFlags (..)
    , prettyOrderFlags
    ) where

import           Control.DeepSeq
import           Data.Aeson
import           Data.Serialize
import           GHC.Generics
import           Text.PrettyPrint

import           Data.Kraken.Util

data OrderFlags
  = Post  -- ^ post-only order (available when ordertype = limit)
  | Fcib  -- ^ prefer fee in base currency (default if selling)
  | Fciq  -- ^ prefer fee in quote currency (default if buying, mutually exclusive with fcib)
  | Nompp -- ^ disable market price protection for market orders
  deriving (Enum, Bounded, Read, Show, Eq, Ord, Generic, NFData, Serialize)


instance ToJSON OrderFlags where
   toJSON = genericToJSON jsonSnakeCase

instance FromJSON OrderFlags where
  parseJSON = genericParseJSON jsonSnakeCase


prettyOrderFlags :: OrderFlags -> Doc
prettyOrderFlags = text . show
