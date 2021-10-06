{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
module Data.Kraken.TradeVolume
    ( TradeVolume (..)
    , prettyTradeVolume
    , prettyTradeVolumeWith
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


data TradeVolume =
  TradeVolume
    { today       :: Double       -- ^ Today
    , last24Hours :: Double -- ^ Last 24 Hours
    }
  deriving (Show, Read, Eq, Ord, Serialize, Generic, ToJSON, NFData)

instance FromJSON TradeVolume where
  parseJSON =
    withArray "Data.Kraken.TradeVolume" $ \arr -> do
      unless (V.length arr == 2) $ fail ("Expected array of length 2, encountedered: " ++ show arr)
      TradeVolume <$> (parseJSON =<< parseStrToNum (arr V.! 0)) <*> (parseJSON =<< parseStrToNum (arr V.! 1))

prettyTradeVolume :: TradeVolume -> Doc
prettyTradeVolume = prettyTradeVolumeWith 0

prettyTradeVolumeWith :: Int -> TradeVolume -> Doc
prettyTradeVolumeWith nesting (TradeVolume t l24) =
  colName "today"           $$ nest n2 (double t) $+$
  colName "last 24 hours"   $$ nest n2 (double l24)
  where n2 = nestCols - nesting
