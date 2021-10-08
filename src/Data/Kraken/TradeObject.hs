{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
module Data.Kraken.TradeObject
    ( TradeObject (..)
    , prettyTradeObject
    ) where


import           Control.DeepSeq
import           Data.Serialize
import           Data.Serialize.Text ()
import qualified Data.Text           as T
import           GHC.Generics
import           Text.PrettyPrint

import           Data.Kraken.Trade
import           Data.Kraken.Types
import           Data.Kraken.Util


data TradeObject = TradeObject
  { tradePairName :: InstrumentName
  , trades        :: [Trade]
  }
  deriving (Read, Show, Eq, Ord, Generic, NFData, Serialize)


prettyTradeObject :: TradeObject -> Doc
prettyTradeObject (TradeObject n p) =
  colName "Trade Pair" $$ nest nestCols (text $ T.unpack n) $+$
  nest nestIndent (vcat $ map (prettyTradeWith nestIndent) p)


