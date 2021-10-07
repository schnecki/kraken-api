{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
module Data.Kraken.TickDataObject
    ( TickDataObject (..)
    , prettyTickDataObject
    ) where


import           Control.DeepSeq
import           Data.Serialize
import qualified Data.Text            as T
import           GHC.Generics
import           Text.PrettyPrint

import           Data.Kraken.TickData
import           Data.Kraken.Types
import           Data.Kraken.Util


data TickDataObject = TickDataObject
  { tickDataName :: InstrumentName
  , tickData     :: [TickData]
  }
  deriving (Read, Show, Eq, Ord, Generic, NFData, Serialize)


prettyTickDataObject :: TickDataObject -> Doc
prettyTickDataObject (TickDataObject instr dts) =
  colName "Tick Name" $$ nest nestCols (text $ T.unpack instr) $+$
  nest nestIndent (vcat $ map (prettyTickDataWith nestIndent) dts)

