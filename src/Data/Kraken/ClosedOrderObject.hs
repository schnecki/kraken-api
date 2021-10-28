{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
module Data.Kraken.ClosedOrderObject
    ( ClosedOrderObject (..)
    , prettyClosedOrderObject
    ) where


import           Control.DeepSeq
import           Data.Serialize
import qualified Data.Text         as T
import           GHC.Generics
import           Text.PrettyPrint

import           Data.Kraken.Order
import           Data.Kraken.Util


data ClosedOrderObject = ClosedOrderObject
  { closedOrderID :: T.Text
  , closedOrder   :: Order
  }
  deriving (Read, Show, Eq, Ord, Generic, NFData, Serialize)


prettyClosedOrderObject :: ClosedOrderObject -> Doc
prettyClosedOrderObject = prettyClosedOrderObjectWith 0


prettyClosedOrderObjectWith :: Int -> ClosedOrderObject -> Doc
prettyClosedOrderObjectWith nesting (ClosedOrderObject oId order) =
  colName "Closed Order ID"    $$ nest n2 (text $ T.unpack oId)  $+$
                                  nest nestIndent' (prettyOrderWith (nesting + nestIndent') order)
  where nestIndent' = nestIndent + nesting
        n2 = nestCols - nesting

