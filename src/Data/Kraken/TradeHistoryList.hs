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
import           Data.Aeson.KeyMap           (toHashMapText)
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
          let oHM = toHashMapText o
          datas <- mapM parseJSON (HM.elems oHM)
          return $ zipWith TradeInfoObject (HM.keys oHM) datas


prettyTradeHistoryList :: TradeHistoryList -> Doc
prettyTradeHistoryList (TradeHistoryList xs count) =
  vcat (map prettyTradeInfoObject xs) $+$
  colName "Number Trades" $$ nest nestCols (int count)
