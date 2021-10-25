{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
module Data.Kraken.OpenOrderList
    ( OpenOrderList (..)
    , prettyOpenOrderList
    ) where


import           Control.DeepSeq
import           Data.Aeson
import qualified Data.HashMap.Strict                as HM (elems, filterWithKey, keys)
import           Data.Serialize
import qualified Data.Vector                        as V
import           GHC.Generics
import           Text.PrettyPrint

import           Data.Kraken.CandlestickGranularity
import           Data.Kraken.OpenOrderObject
import           Data.Kraken.Util


data OpenOrderList =
  OpenOrderList
    { openOrders :: [OpenOrderObject]
    }
  deriving (Read, Show, Eq, Ord, Generic, NFData, Serialize)


instance FromJSON OpenOrderList where
  parseJSON =
    withObject "Data.Kraken.OpenOrderList" $ \o -> do
      datas <- mapM parseJSON (HM.elems o)
      return $ OpenOrderList $ zipWith OpenOrderObject (HM.keys o) datas

prettyOpenOrderList :: OpenOrderList -> Doc
prettyOpenOrderList (OpenOrderList xs) = vcat (map prettyOpenOrderObject xs)
