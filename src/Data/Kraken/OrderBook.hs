{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
module Data.Kraken.OrderBook
    ( OrderBook (..)
    , prettyOrderBook
    , prettyOrderBookWith
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

data OrderBook =
  OrderBook
    { price  :: PriceValue -- ^ Price of order
    , volume :: Double     -- ^ Volume of order
    , time   :: DateTime   -- ^ Time of order
    }
  deriving (Read, Show, Eq, Ord, Generic, NFData, ToJSON, Serialize)


instance FromJSON OrderBook where
  parseJSON =
    withArray "Data.Kraken.OrderBook" $ \arr -> do
      unless (V.length arr == 3) $ fail ("Expected an array of length 3, but encountered: " ++ show arr)
      pr <- parseJSON =<< parseStrToNum (arr V.! 0)
      vo <- parseJSON =<< parseStrToNum (arr V.! 1)
      ti <- secondsToDateTime <$> parseJSON (arr V.! 2)
      return $ OrderBook pr vo ti


prettyOrderBook :: OrderBook -> Doc
prettyOrderBook = prettyOrderBookWith 0

prettyOrderBookWith :: Int -> OrderBook -> Doc
prettyOrderBookWith nesting order =
  colName "Price"   $$ nest n2 (prettyPriceValue $ price order) $+$
  colName "Volume"    $$ nest n2 (double $ volume order)  $+$
  colName "Time"    $$ nest n2 (prettyDateTime $ time order)
  where n2 = nestCols - nesting
