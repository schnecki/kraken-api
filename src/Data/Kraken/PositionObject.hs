{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
module Data.Kraken.PositionObject
    ( PositionObject (..)
    , prettyPositionObject
    ) where


import           Control.DeepSeq
import           Data.Serialize
import qualified Data.Text            as T
import           GHC.Generics
import           Text.PrettyPrint

import           Data.Kraken.Position
import           Data.Kraken.Util


data PositionObject = PositionObject
  { positionID :: T.Text
  , position   :: Position
  }
  deriving (Read, Show, Eq, Ord, Generic, NFData, Serialize)


prettyPositionObject :: PositionObject -> Doc
prettyPositionObject = prettyPositionObjectWith 0


prettyPositionObjectWith :: Int -> PositionObject -> Doc
prettyPositionObjectWith nesting (PositionObject pId pos) =
  colName "Position ID"    $$ nest n2 (text $ T.unpack pId)  $+$
                                   nest nestIndent' (prettyPositionWith (nesting + nestIndent') pos)
  where n2 = nestCols - nesting
        nestIndent' = nestIndent + nesting

