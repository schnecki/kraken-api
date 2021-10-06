{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
module Data.Kraken.TradableAssetPairObject
    ( TradableAssetPairObject (..)
    , prettyTradableAssetPairObject
    ) where


import           Control.DeepSeq
import           Data.Serialize
import           Data.Serialize.Text           ()
import qualified Data.Text                     as T
import           GHC.Generics
import           Text.PrettyPrint

import           Data.Kraken.TradableAssetPair
import           Data.Kraken.Types
import           Data.Kraken.Util


data TradableAssetPairObject = TradableAssetPairObject
  { assetPairName :: InstrumentName
  , assetPair     :: TradableAssetPair
  }
  deriving (Read, Show, Eq, Ord, Generic, NFData, Serialize)


prettyTradableAssetPairObject :: TradableAssetPairObject -> Doc
prettyTradableAssetPairObject (TradableAssetPairObject n p) =
  colName "Asset Pair Name" $$ nest nestCols (text $ T.unpack n) $+$
  -- colName "Asset Pair"      $$
  nest nestIndent (prettyTradableAssetPairWith nestIndent p)


