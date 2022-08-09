{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Data.Kraken.TradeInfoList
    ( TradeInfoList (..)
    , prettyTradeInfoList
    ) where

import           Control.DeepSeq
import           Data.Aeson
import qualified Data.HashMap.Strict         as HM (elems, keys, lookup)
import           Data.Serialize
import           EasyLogger
import           GHC.Generics
import           Text.PrettyPrint

import           Data.Kraken.TradeInfoObject


import           Debug.Trace

data TradeInfoList =
  TradeInfoList
    { tradeInfoObjects :: [TradeInfoObject]
    }
  deriving (Read, Show, Eq, Ord, Generic, NFData, Serialize)

instance FromJSON TradeInfoList where
  parseJSON =
    withObject "Data.Kraken.TradeInfoList" $ \o ->
      $(pureLogDebug) ("Data.Kraken.TradeInfoList parseJSON input: " ++ show o) $ do
        datas <- mapM parseJSON (HM.elems o)
        return $ TradeInfoList $ zipWith TradeInfoObject (HM.keys o) datas

prettyTradeInfoList :: TradeInfoList -> Doc
prettyTradeInfoList (TradeInfoList xs) = vcat (map prettyTradeInfoObject xs)
