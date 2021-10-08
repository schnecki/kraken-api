{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
module Data.Kraken.SpreadObject
    ( SpreadObject (..)
    , prettySpreadObject
    ) where


import           Control.DeepSeq
import           Data.Serialize
import           Data.Serialize.Text ()
import qualified Data.Text           as T
import           GHC.Generics
import           Text.PrettyPrint

import           Data.Kraken.Spread
import           Data.Kraken.Types
import           Data.Kraken.Util


data SpreadObject = SpreadObject
  { spreadPairName :: InstrumentName
  , spreads        :: [Spread]
  }
  deriving (Read, Show, Eq, Ord, Generic, NFData, Serialize)


prettySpreadObject :: SpreadObject -> Doc
prettySpreadObject (SpreadObject n p) =
  colName "Spread Pair" $$ nest nestCols (text $ T.unpack n) $+$
  nest nestIndent (vcat $ map (prettySpreadWith nestIndent) p)


