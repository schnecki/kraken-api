{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
module Data.Kraken.AssetInfo
    ( AssetInfo (..)
    ) where


import           Control.DeepSeq
import           Data.Aeson
import           Data.Aeson.Casing
import           Data.HashMap.Strict    (elems, keys)
import           Data.Serialize
import           Data.Serialize.Text
import qualified Data.Text              as T
import           GHC.Generics

import           Data.Kraken.AssetClass
import           Data.Kraken.Types
import           Data.Kraken.Util

import           Debug.Trace

data AssetInfo =
  AssetInfo
    { aclass          :: AssetClass
    , altname         :: InstrumentName
    , decimals        :: Int
    , displayDecimals :: Int
    }
  deriving (Read, Show, Eq, Ord, Generic, NFData, Serialize)


instance ToJSON AssetInfo where
   toJSON = genericToJSON jsonSnakeCase

instance FromJSON AssetInfo where
  parseJSON = genericParseJSON jsonSnakeCase


