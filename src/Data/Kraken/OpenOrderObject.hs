{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
module Data.Kraken.OpenOrderObject
    ( OpenOrderObject (..)
    , prettyOpenOrderObject
    ) where


import           Control.DeepSeq
import           Data.Serialize
import qualified Data.Text             as T
import           GHC.Generics
import           Text.PrettyPrint

import           Data.Kraken.OpenOrder
import           Data.Kraken.Util


data OpenOrderObject = OpenOrderObject
  { openOrderID :: T.Text
  , openOrder   :: OpenOrder
  }
  deriving (Read, Show, Eq, Ord, Generic, NFData, Serialize)


prettyOpenOrderObject :: OpenOrderObject -> Doc
prettyOpenOrderObject = prettyOpenOrderObjectWith 0


prettyOpenOrderObjectWith :: Int -> OpenOrderObject -> Doc
prettyOpenOrderObjectWith nesting (OpenOrderObject oId order) =
  colName "Open Order ID"    $$ nest n2 (text $ T.unpack oId)  $+$
                                nest n2 (prettyOpenOrder order)
  where n2 = nestCols - nesting
