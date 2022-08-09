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
  | Viqc  -- ^ volume expressed in quote currency. This is supported only for market orders.
  deriving (Enum, Bounded, Read, Eq, Ord, Generic, NFData, Serialize)

instance Show OrderFlags where
  show Post  = "post"
  show Fcib  = "fcib"
  show Fciq  = "fciq"
  show Nompp = "nompp"
  show Viqc  = "viqc"


instance ToJSON OrderFlags where
   toJSON = genericToJSON lowerCase

instance FromJSON OrderFlags where
  parseJSON = genericParseJSON lowerCase


prettyOrderFlags :: OrderFlags -> Doc
prettyOrderFlags = text . show
