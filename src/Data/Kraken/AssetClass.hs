{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
module Data.Kraken.AssetClass
    ( AssetClass (..)

    ) where


import           Control.DeepSeq
import           Data.Aeson
import           Data.Aeson.Casing
import           Data.ByteString.Lazy (fromStrict)
import           Data.Char
import qualified Data.HashMap.Strict  as HM
import           Data.Serialize
import qualified Data.Text            as T
import           Data.Text.Encoding   as TSE
import           GHC.Generics
import           Text.Read            (readMaybe)

import           Data.Kraken.Util

import           Debug.Trace

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

