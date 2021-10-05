{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
module Data.Kraken.AssetInfoObject
    ( AssetInfoObject (..)
    ) where


import           Control.DeepSeq
import           Data.Aeson
import           Data.HashMap.Strict    (elems, keys)
import           Data.Serialize
import           Data.Serialize.Text
import qualified Data.Text              as T
import           GHC.Generics

import           Data.Kraken.AssetClass
import           Data.Kraken.AssetInfo
import           Data.Kraken.Types
import           Data.Kraken.Util

import           Debug.Trace


data AssetInfoObject = AssetInfoObject
  { assetName :: InstrumentName
  , assetInfo :: AssetInfo
  }
  deriving (Read, Show, Eq, Ord, Generic, NFData, Serialize)

