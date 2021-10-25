{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
module Data.Kraken.OrderDescription
    ( OrderDescription (..)
    , prettyOrderDescription
    , prettyOrderDescriptionWith
    ) where


import           Control.DeepSeq
import           Control.Monad              (unless)
import           Data.Aeson
import           Data.Aeson.Types
import           Data.Kraken.OrderType
import           Data.Kraken.TradeOrderType
import           Data.Serialize
import           Data.Serialize.Text        ()
import qualified Data.Text                  as T
import qualified Data.Vector                as V
import           GHC.Generics
import           Text.PrettyPrint

import           Data.Kraken.PriceValue
import           Data.Kraken.Util

data OrderDescription =
  OrderDescription
    { pair      :: T.Text         -- ^ asset pair
    , tradeType :: OrderType      -- ^ order type (sell/buy)
    , orderType :: TradeOrderType -- ^ order type
    , price     :: PriceValue     -- ^ decimal primary price
    , price2    :: PriceValue     -- ^ decimal secondary price
    , leverage  :: Double         -- ^ decimal amount of leverage
    , order     :: T.Text         -- ^ order description
    , close     :: T.Text         -- ^ conditional close order description (if conditional close set)
    }
  deriving (Read, Show, Eq, Ord, Generic, NFData, ToJSON, Serialize)


instance FromJSON OrderDescription where
  parseJSON =
    withArray "Data.Kraken.OrderDescription" $ \arr -> do
      unless (V.length arr == 8) $ fail ("Expected an array of length 8, but encountered: " ++ show arr)
      pa <- parseJSON (arr V.! 0)
      tT <- parseJSON (arr V.! 1)
      oT <- parseJSON (arr V.! 2)
      p1 <- parseJSON (arr V.! 3)
      p2 <- parseJSON (arr V.! 4)
      leText <- parseJSON (arr V.! 5) :: Parser T.Text
      case T.splitOn ":" leText of
        [l1, l2] -> do
          le1 <- parseJSON (String l1) :: Parser Double
          le2 <- parseJSON (String l2) :: Parser Double
          let le = le1 / le2
          ord <- parseJSON =<< parseStrToNum (arr V.! 6)
          cl <- parseJSON (arr V.! 7)
          return $ OrderDescription pa tT oT p1 p2 le ord cl
        _ -> fail $ "Expected a leverage in the form of x:y, but saw " ++ show leText


prettyOrderDescription :: OrderDescription -> Doc
prettyOrderDescription = prettyOrderDescriptionWith 0

prettyOrderDescriptionWith :: Int -> OrderDescription -> Doc
prettyOrderDescriptionWith nesting desc =
  colName "pair"      $$ nest n2 (text $ T.unpack $ pair desc) $+$
  colName "tradeType" $$ nest n2 (prettyOrderType $ tradeType desc) $+$
  colName "orderType" $$ nest n2 (prettyTradeOrderType $ orderType desc) $+$
  colName "price"     $$ nest n2 (prettyPriceValue $ price desc) $+$
  colName "price2"    $$ nest n2 (prettyPriceValue $ price2 desc) $+$
  colName "leverage"  $$ nest n2 (double $ leverage desc) $+$
  colName "order"     $$ nest n2 (text $ T.unpack $ order desc) $+$
  colName "close"     $$ nest n2 (text $ T.unpack $ close desc)
  where n2 = nestCols - nesting
