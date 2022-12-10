{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
module Data.Kraken.Trade
    ( Trade (..)
    , prettyTrade
    , prettyTradeWith
    ) where


import           Control.DeepSeq
import           Control.Monad              (unless)
import           Data.Aeson
import           Data.Serialize
import           Data.Serialize.Text        ()
import qualified Data.Text                  as T
import qualified Data.Vector                as V
import           GHC.Generics
import           Text.PrettyPrint

import           Data.Kraken.DateTime
import           Data.Kraken.OrderType
import           Data.Kraken.PriceValue
import           Data.Kraken.TradeOrderType
import           Data.Kraken.Util

data Trade =
  Trade
    { price          :: PriceValue
    , volume         :: Double
    , time           :: DateTime
    , tradeType      :: OrderType
    , tradeOrderType :: TradeOrderType
    , miscellaneous  :: T.Text
    }
  deriving (Read, Show, Eq, Ord, Generic, NFData, Serialize)

-- [String \"16775.00000\",
--  , String \"0.00011010\",
--  , Number 1.668546834422194e9,
--  , String \"s\",
--  , String \"m\",
--  , String \"\",
--  , Number 5.328663e7
-- ]"


instance FromJSON Trade where
  parseJSON =
    withArray "Data.Kraken.Trade" $ \arr -> do
      unless (V.length arr == 6) $ warn "Trade" ("Expected an array of length 6, but encountered: " ++ show arr)
      -- when (V.length arr == 7) $ fail ("Expected an array of length 6, but encountered: " ++ show arr)
      pr <- parseJSON (arr V.! 0)
      vo <- parseJSON =<< parseStrToNum (arr V.! 1)
      ti <- posixTimeToDateTime . fromRational <$> parseJSON (arr V.! 2)
      tp <- parseJSON (arr V.! 3)
      ot <- parseJSON (arr V.! 4)
      mi <- parseJSON (arr V.! 5)
      -- (unknown :: Double) <- parseJSON (arr V.! 6)
      return $ Trade pr vo ti tp ot mi


prettyTrade :: Trade -> Doc
prettyTrade = prettyTradeWith 0

prettyTradeWith :: Int -> Trade -> Doc
prettyTradeWith nesting trade =
  colName "price"          $$ nest n2 (prettyPriceValue $ price trade) $+$
  colName "volume"         $$ nest n2 (double $ volume trade) $+$
  colName "time"           $$ nest n2 (prettyDateTime $ time trade) $+$
  colName "tradeType"      $$ nest n2 (prettyOrderType $ tradeType trade) $+$
  colName "tradeOrderType" $$ nest n2 (prettyTradeOrderType $ tradeOrderType trade) $+$
  colName "miscellaneous"  $$ nest n2 (text $ T.unpack $ miscellaneous trade)
  where n1 = 2 * nestIndent
        n1Sum = nesting + n1
        n2 = nestCols - nesting
