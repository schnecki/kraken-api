{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
module Data.Kraken.AssetInfo
    ( AssetInfo (..)
    , prettyAssetInfo
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
prettyAssetInfo info =

  colName "aclass"          $$ nest nestCols (prettyAssetClass $ aclass info) $+$
  colName "altname"         $$ nest nestCols (text $ T.unpack $ altname info) $+$
  colName "decimals"        $$ nest nestCols (int $ decimals info) $+$
  colName "displayDecimals" $$ nest nestCols (int $ displayDecimals info)
