{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
module Data.Kraken.TradeCount
    ( TradeCount (..)
    , prettyTradeCount
    , prettyTradeCountWith
    ) where

import           Control.DeepSeq
import           Control.Monad       (unless)
import           Data.Aeson
import           Data.Serialize
import           Data.Serialize.Text ()
import qualified Data.Vector         as V
import           GHC.Generics
import           Text.PrettyPrint

import           Data.Kraken.Util


data TradeCount =
  TradeCount
    { today       :: Int       -- ^ Today
    , last24Hours :: Int -- ^ Last 24 Hours
    }
  deriving (Show, Read, Eq, Ord, Serialize, Generic, ToJSON, NFData)


instance FromJSON TradeCount where
  parseJSON =
    withArray "Data.Kraken.TradeCount" $ \arr -> do
      unless (V.length arr == 2) $ fail ("Expected array of length 2, encountedered: " ++ show arr)
      TradeCount <$> (parseJSON =<< parseStrToNum (arr V.! 0)) <*> (parseJSON =<< parseStrToNum (arr V.! 1))


prettyTradeCount :: TradeCount -> Doc
prettyTradeCount = prettyTradeCountWith 0

prettyTradeCountWith :: Int -> TradeCount -> Doc
prettyTradeCountWith nesting (TradeCount t l24) =
  colName "today"           $$ nest n2 (int t) $+$
  colName "last 24 hours"   $$ nest n2 (int l24)
  where n2 = nestCols - nesting

