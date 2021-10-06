{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
module Data.Kraken.AssetInfo
    ( AssetInfo (..)
    , prettyAssetInfo
    , prettyAssetInfoWith
    ) where


import           Control.DeepSeq
import           Data.Aeson
import           Data.Serialize
import           Data.Serialize.Text    ()
import qualified Data.Text              as T
import           GHC.Generics
import           Text.PrettyPrint

import           Data.Kraken.AssetClass
import           Data.Kraken.Types
import           Data.Kraken.Util


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


prettyAssetInfo :: AssetInfo -> Doc
prettyAssetInfo = prettyAssetInfoWith 0

prettyAssetInfoWith :: Int -> AssetInfo -> Doc
prettyAssetInfoWith nesting info =
  colName "aclass"          $$ nest n2 (prettyAssetClass $ aclass info) $+$
  colName "altname"         $$ nest n2 (text $ T.unpack $ altname info) $+$
  colName "decimals"        $$ nest n2 (int $ decimals info) $+$
  colName "displayDecimals" $$ nest n2 (int $ displayDecimals info)
  where n2 = nestCols - nesting
