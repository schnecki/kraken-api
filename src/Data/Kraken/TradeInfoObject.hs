{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
module Data.Kraken.TradeInfoObject
    ( TradeInfoObject (..)
    , prettyTradeInfoObject
    ) where

import           Control.DeepSeq
import           Data.Serialize
import qualified Data.Text             as T
import           GHC.Generics
import           Text.PrettyPrint

import           Data.Kraken.TradeInfo
import           Data.Kraken.Util


data TradeInfoObject = TradeInfoObject
  { tradeId    :: T.Text
  , tradeInfos :: TradeInfo
  }
  deriving (Read, Show, Eq, Ord, Generic, NFData, Serialize)


prettyTradeInfoObject :: TradeInfoObject -> Doc
prettyTradeInfoObject = prettyTradeInfoObjectWith 0


prettyTradeInfoObjectWith :: Int -> TradeInfoObject -> Doc
prettyTradeInfoObjectWith nesting (TradeInfoObject oId order) =
  colName "Trade ID"    $$ nest n2 (text $ T.unpack oId)  $+$
                           nest nestIndent' (prettyTradeInfoWith (nesting + nestIndent') order)
  where n2 = nestCols - nesting
        nestIndent' = nestIndent + nesting
