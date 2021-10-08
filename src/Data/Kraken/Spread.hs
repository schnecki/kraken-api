{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
module Data.Kraken.Spread
    ( Spread (..)
    , prettySpread
    , prettySpreadWith
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


data Spread =
  Spread
    { time :: DateTime
    , bid  :: PriceValue
    , ask  :: PriceValue
    }
  deriving (Read, Show, Eq, Ord, Generic, NFData, Serialize)


instance FromJSON Spread where
  parseJSON =
    withArray "Data.Kraken.Spread" $ \arr -> do
      unless (V.length arr == 3) $ fail ("Expected an array of length 3, but encountered: " ++ show arr)
      ti <- posixTimeToDateTime <$> parseJSON (arr V.! 0)
      bi <- parseJSON =<< parseStrToNum (arr V.! 1)
      as <- parseJSON =<< parseStrToNum (arr V.! 2)
      return $ Spread ti bi as


prettySpread :: Spread -> Doc
prettySpread = prettySpreadWith 0

prettySpreadWith :: Int -> Spread -> Doc
prettySpreadWith nesting spread =
  colName "time"          $$ nest n2 (prettyDateTime $ time spread)  $+$
  colName "bid"           $$ nest n2 (prettyPriceValue $ bid spread) $+$
  colName "ask"           $$ nest n2 (prettyPriceValue $ ask spread)
  where n2 = nestCols - nesting
