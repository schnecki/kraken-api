{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
module Data.Kraken.OrderBookObject
    ( OrderBookObject (..)
    , prettyOrderBookObject
    ) where


import           Control.DeepSeq
import           Data.Serialize
import qualified Data.Text             as T
import           GHC.Generics
import           Text.PrettyPrint

import           Data.Kraken.OrderBook
import           Data.Kraken.Types
import           Data.Kraken.Util


data OrderBookObject =
  OrderBookObject
    { orderBookPairName :: InstrumentName
    , asks              :: [OrderBook]
    , bids              :: [OrderBook]
    }
  deriving (Read, Show, Eq, Ord, Generic, NFData, Serialize)


prettyOrderBookObject :: OrderBookObject -> Doc
prettyOrderBookObject (OrderBookObject n as bs) =
  colName "Order Book Pair" $$ nest nestIndent (text $ show n) $+$
  colName "Asks"            $$ nest nestIndent (vcat $ map (prettyOrderBookWith nestIndent) as) $+$
  colName "Bids"            $$ nest nestIndent (vcat $ map (prettyOrderBookWith nestIndent) bs)

