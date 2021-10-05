{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
module Data.Kraken.AssetInfoList
    ( AssetInfoList (..)
    ) where


import           Control.DeepSeq
import           Data.Aeson
import           Data.HashMap.Strict         (elems, keys)
import           Data.Serialize
import           Data.Serialize.Text
import qualified Data.Text                   as T
import           GHC.Generics

import           Data.Kraken.AssetClass
import           Data.Kraken.AssetInfoObject
import           Data.Kraken.Types
import           Data.Kraken.Util

import           Debug.Trace

data AssetInfoList =
  AssetInfoList
    { assetInfos :: [AssetInfoObject]
    }
  deriving (Read, Show, Eq, Ord, Generic, NFData, Serialize)


instance FromJSON AssetInfoList where
  parseJSON val = withObject "Data.Kraken.AssetInfoList" (\o -> AssetInfoList . zipWith AssetInfoObject (keys o) <$> mapM parseJSON (elems o)) val

