{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
module Data.Kraken.TradableAssetPairList
    ( TradableAssetPairList(..)
    , prettyTradableAssetPairList
    ) where


import           Control.DeepSeq
import           Data.Aeson
import           Data.HashMap.Strict                 (elems, keys)
import           Data.Serialize
import           GHC.Generics
import           Text.PrettyPrint

import           Data.Kraken.TradableAssetPairObject


data TradableAssetPairList =
  TradableAssetPairList
    { assetInfos :: [TradableAssetPairObject]
    }
  deriving (Read, Show, Eq, Ord, Generic, NFData, Serialize)


instance FromJSON TradableAssetPairList where
  parseJSON val = withObject "Data.Kraken.TradableAssetPairList" (\o -> TradableAssetPairList . zipWith TradableAssetPairObject (keys o) <$> mapM parseJSON (elems o)) val


prettyTradableAssetPairList :: TradableAssetPairList -> Doc
prettyTradableAssetPairList (TradableAssetPairList xs) = vcat (map prettyTradableAssetPairObject xs)
