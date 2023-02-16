{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
module Data.Kraken.AssetInfoList
    ( AssetInfoList (..)
    , prettyAssetInfoList
    ) where


import           Control.DeepSeq
import           Data.Aeson
import           Data.Aeson.Key
import qualified Data.Aeson.KeyMap           as HM
import           Data.Serialize
import           GHC.Generics
import           Text.PrettyPrint

import           Data.Kraken.AssetInfoObject


data AssetInfoList =
  AssetInfoList
    { assetInfos :: [AssetInfoObject]
    }
  deriving (Read, Show, Eq, Ord, Generic, NFData, Serialize)


instance FromJSON AssetInfoList where
  parseJSON val = withObject "Data.Kraken.AssetInfoList" (\o -> AssetInfoList . zipWith AssetInfoObject (map toText . HM.keys $ o) <$> mapM parseJSON (HM.elems o)) val


prettyAssetInfoList :: AssetInfoList -> Doc
prettyAssetInfoList (AssetInfoList xs) = vcat (map prettyAssetInfoObject xs)
