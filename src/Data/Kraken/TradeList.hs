{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
module Data.Kraken.TradeList
    ( TradeList(..)
    , prettyTradeList
    , allTrades
    ) where

import           Control.DeepSeq
import           Data.Aeson
import           Data.Aeson.KeyMap       (toHashMapText)
import qualified Data.HashMap.Strict     as HM (elems, filterWithKey, keys)
import           Data.Serialize
import qualified Data.Vector             as V
import           GHC.Generics
import           Text.PrettyPrint

import           Data.Kraken.Trade
import           Data.Kraken.TradeObject
import           Data.Kraken.Util

data TradeList =
  TradeList
    { tradeLast :: Integer
    , tradeList :: [TradeObject]
    }
  deriving (Read, Show, Eq, Ord, Generic, NFData, Serialize)

allTrades :: TradeList -> [Trade]
allTrades = concatMap trades . tradeList


instance FromJSON TradeList where
  parseJSON =
    withObject "Data.Kraken.TradeList" $ \o -> do
      last' <- parseJSON =<< parseStrToNum =<< (o .: "last")
      let oHM = toHashMapText o
      datas <- mapM (withArray "Data.Kraken.TradeList parsing Trades" $ \o' -> V.toList <$> mapM parseJSON o') (mkElems oHM)
      return $ TradeList last' $ zipWith TradeObject (mkKeys oHM) datas
    where
      mkKeys o = filter (/= "last") (HM.keys o)
      mkElems o = HM.elems $ HM.filterWithKey (\k _ -> k /= "last") o


prettyTradeList :: TradeList -> Doc
prettyTradeList (TradeList last' xs) =
  colName "Trade Last ID" $$ nest nestCols (integer last') $+$
  vcat (map prettyTradeObject xs)
