{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
module Data.Kraken.TickData
    ( TickData (..)
    , prettyTickData
    , prettyTickDataWith
    ) where


import           Control.DeepSeq
import           Control.Monad          (unless)
import           Data.Aeson
import           Data.Serialize
import           Data.Serialize.Text    ()
import qualified Data.Vector            as V
import           GHC.Generics
import           Text.PrettyPrint

import           Data.Kraken.DateTime
import           Data.Kraken.PriceValue
import           Data.Kraken.Util

data TickData =
  TickData
    { time   :: DateTime
    , open   :: PriceValue
    , high   :: PriceValue
    , low    :: PriceValue
    , close  :: PriceValue
    , vwap   :: PriceValue -- ^ Volume weighted average price
    , volume :: Double     -- ^ Volume
    , count  :: Int        -- ^ Trade count
    }
  deriving (Read, Show, Eq, Ord, Generic, NFData, ToJSON, Serialize)


instance FromJSON TickData where
  parseJSON =
    withArray "Data.Kraken.TickData" $ \arr -> do
      unless (V.length arr == 8) $ fail ("Expected an array of length 8, but encountered: " ++ show arr)
      ti <- secondsToDateTime <$> parseJSON (arr V.! 0)
      op <- parseJSON (arr V.! 1)
      hi <- parseJSON (arr V.! 2)
      lo <- parseJSON (arr V.! 3)
      cl <- parseJSON (arr V.! 4)
      vw <- parseJSON (arr V.! 5)
      vo <- parseJSON =<< parseStrToNum (arr V.! 6)
      co <- parseJSON (arr V.! 7)
      return $ TickData ti op hi lo cl vw vo co


prettyTickData :: TickData -> Doc
prettyTickData = prettyTickDataWith 0

prettyTickDataWith :: Int -> TickData -> Doc
prettyTickDataWith nesting info =
  colName "time"    $$ nest n2 (prettyDateTime $ time info)  $+$
  colName "open"    $$ nest n2 (prettyPriceValue $ open info)  $+$
  colName "high"    $$ nest n2 (prettyPriceValue $ high info)  $+$
  colName "low"     $$ nest n2 (prettyPriceValue $ low info)  $+$
  colName "close"   $$ nest n2 (prettyPriceValue $ close info)  $+$
  colName "vwap"    $$ nest n2 (prettyPriceValue $ vwap info)  $+$
  colName "volume"  $$ nest n2 (double $ volume info)  $+$
  colName "count"   $$ nest n2 (int $ count info)
  where n2 = nestCols - nesting
