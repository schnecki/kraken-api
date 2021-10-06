{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
module Data.Kraken.AssetClass
    ( AssetClass (..)
    , prettyAssetClass
    ) where


import           Control.DeepSeq
import           Data.Aeson
import           Data.Aeson.Casing
import           Data.Serialize
import           GHC.Generics
import           Text.PrettyPrint  (Doc, text)
import           Text.Read         (readMaybe)

import           Data.Kraken.Util


data AssetClass =
  Currency -- ^ Currency
  deriving (Enum, Bounded, Read, Show, Eq, Ord, Generic, NFData, Serialize)


instance ToJSON AssetClass where
   toJSON = genericToJSON jsonSnakeCase

instance FromJSON AssetClass where
  parseJSON json@(String v) = withText "Data.Kraken.AssetClass.AssetClass" parseString json
    where parseString str = maybe (fail "Data.Kraken.AssetClass.AssetClass") return (readMaybe $ mkWord str)
          mkWord = pascalCase . filter (/= '"') .show
  parseJSON v = genericParseJSON jsonSnakeCase v


prettyAssetClass :: AssetClass -> Doc
prettyAssetClass = text . show
