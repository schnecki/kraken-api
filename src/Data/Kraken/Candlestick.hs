{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
module Data.Kraken.Candlestick
    ( Candlestick (..)
    , prettyCandlestick
    , prettyCandlestickWith
    , parseCandlestickFromList
    ) where

import           Control.DeepSeq
import           Data.Aeson
import           Data.Aeson.Types
import           Data.Serialize
import           Data.Serialize.Text    ()
import qualified Data.Vector            as V
import           GHC.Generics
import           Text.PrettyPrint

import           Data.Kraken.PriceValue
import           Data.Kraken.Util

data Candlestick =
  Candlestick
    { price          :: PriceValue   -- ^ Price
    , volumeWholeLot :: Maybe Double -- ^ Whole lot volume
    , volumeLot      :: Double       -- ^ Lot Volume
    }
  deriving (Show, Read, Eq, Ord, Serialize, ToJSON, Generic, NFData)

instance FromJSON Candlestick where
  parseJSON = parseCandlestickFromList


prettyCandlestick :: Candlestick -> Doc
prettyCandlestick = prettyCandlestickWith 0

prettyCandlestickWith :: Int -> Candlestick -> Doc
prettyCandlestickWith nesting (Candlestick p w l) =
  colName "price"                         $$ nest n2 (prettyPriceValue p) $+$
  mVal w (\v ->colName "Whole Lot Volume" $$ nest n2 (double v)) $+$
  colName "Lot Volume"                    $$ nest n2 (double l)
  where n2 = nestCols - nesting


parseCandlestickFromList :: Value -> Parser Candlestick
parseCandlestickFromList =
  withArray "Data.Kraken.Candlestick: Array of Doubles" $ \arr ->
    if V.length arr == 3
      then Candlestick <$> parseJSON (arr V.! 0) <*> (Just <$> (parseJSON =<< parseStrToNum (arr V.! 1) :: Parser Double)) <*> (parseJSON =<< parseStrToNum (arr V.! 2))
      else if V.length arr == 2
             then Candlestick <$> parseJSON (arr V.! 0) <*> pure Nothing <*> (parseJSON =<< parseStrToNum (arr V.! 1))
             else fail $ "expected an array of length 2 or 3. ecountered: " ++ show arr
