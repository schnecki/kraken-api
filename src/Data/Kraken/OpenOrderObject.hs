{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
module Data.Kraken.OpenOrderObject
    ( OpenOrderObject (..)
    , prettyOpenOrderObject
    ) where


import           Control.DeepSeq
import           Data.Serialize
import qualified Data.Text         as T
import           GHC.Generics
import           Text.PrettyPrint

import           Data.Kraken.Order
import           Data.Kraken.Util


data OpenOrderObject = OpenOrderObject
  { openOrderID :: T.Text
  , openOrder   :: Order
  }
  deriving (Read, Show, Eq, Ord, Generic, NFData, Serialize)


prettyOpenOrderObject :: OpenOrderObject -> Doc
prettyOpenOrderObject = prettyOpenOrderObjectWith 0


prettyOpenOrderObjectWith :: Int -> OpenOrderObject -> Doc
prettyOpenOrderObjectWith nesting (OpenOrderObject oId order) =
  colName "Open Order ID"    $$ nest n2 (text $ T.unpack oId)  $+$
                                nest nestIndent' (prettyOrderWith (nesting + nestIndent') order)
  where n2 = nestCols - nesting
        nestIndent' = nestIndent + nesting
