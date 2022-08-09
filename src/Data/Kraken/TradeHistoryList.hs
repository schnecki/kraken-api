{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Data.Kraken.TradeHistoryList
    ( TradeHistoryList(..)
    , prettyTradeHistoryList
    , allTradeInfos
    ) where

import           Control.DeepSeq
import           Data.Aeson
import qualified Data.HashMap.Strict         as HM (elems, filterWithKey, keys)
import           Data.Serialize
import           EasyLogger
import           GHC.Generics
import           Text.PrettyPrint

import           Data.Kraken.TradeInfo
import           Data.Kraken.TradeInfoObject
import           Data.Kraken.Util

data TradeHistoryList =
  TradeHistoryList
    { tradeHistoryList  :: [TradeInfoObject]
    , tradeHistoryCount :: Int
    }
  deriving (Read, Show, Eq, Ord, Generic, NFData, Serialize)

allTradeInfos :: TradeHistoryList -> [TradeInfo]
allTradeInfos = map tradeInfo . tradeHistoryList


instance FromJSON TradeHistoryList where
  parseJSON =
    withObject "Data.Kraken.TradeHistoryList" $ \o ->
      $(pureLogDebug) ("Data.Kraken.TradeHistoryList parseJSON input: " ++ show o) $ do
        count <- parseJSON =<< parseStrToNum =<< (o .: "count")
        trades <- (o .: "trades") >>= withObject "Data.Kraken.TradeInfoObject" parseTradeInfoObjects
        return $  TradeHistoryList trades count
    where
      parseTradeInfoObjects o = do
          datas <- mapM parseJSON (HM.elems o)
          return $ zipWith TradeInfoObject (HM.keys o) datas


prettyTradeHistoryList :: TradeHistoryList -> Doc
prettyTradeHistoryList (TradeHistoryList xs count) =
  vcat (map prettyTradeInfoObject xs) $+$
  colName "Number Trades" $$ nest nestCols (int count)
