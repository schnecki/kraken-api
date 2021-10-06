{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
module Data.Kraken.AssetInfoObject
    ( AssetInfoObject (..)
    , prettyAssetInfoObject
    ) where


import           Control.DeepSeq
import           Data.Serialize
import qualified Data.Text             as T
import           GHC.Generics
import           Text.PrettyPrint

import           Data.Kraken.AssetInfo
import           Data.Kraken.Types
import           Data.Kraken.Util


data AssetInfoObject = AssetInfoObject
  { assetName :: InstrumentName
  , assetInfo :: AssetInfo
  }
  deriving (Read, Show, Eq, Ord, Generic, NFData, Serialize)


prettyAssetInfoObject :: AssetInfoObject -> Doc
prettyAssetInfoObject (AssetInfoObject instr info) =
  colName "Asset Name" $$ nest nestCols (text $ T.unpack instr) $+$
  colName "Asset Info"  $$ nest nestCols (prettyAssetInfo info)
