{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
module Data.Kraken.TickerVolume
    ( TickerVolume (..)
    , prettyTickerVolume
    , prettyTickerVolumeWith
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


data TickerVolume =
  TickerVolume
    { today       :: Double -- ^ Today
    , last24Hours :: Double -- ^ Last 24 Hours
    }
  deriving (Show, Read, Eq, Ord, Serialize, Generic, ToJSON, NFData)

instance FromJSON TickerVolume where
  parseJSON =
    withArray "Data.Kraken.TickerVolume" $ \arr -> do
      unless (V.length arr == 2) $ fail ("Expected array of length 2, encountedered: " ++ show arr)
      TickerVolume <$> (parseJSON =<< parseStrToNum (arr V.! 0)) <*> (parseJSON =<< parseStrToNum (arr V.! 1))

prettyTickerVolume :: TickerVolume -> Doc
prettyTickerVolume = prettyTickerVolumeWith 0

prettyTickerVolumeWith :: Int -> TickerVolume -> Doc
prettyTickerVolumeWith nesting (TickerVolume t l24) =
  colName "today"           $$ nest n2 (double t) $+$
  colName "last 24 hours"   $$ nest n2 (double l24)
  where n2 = nestCols - nesting
