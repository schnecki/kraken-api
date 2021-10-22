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
      open' <- parseJSON =<< parseStrToNum =<< (o .: "last")
      datas <- mapM (withArray "Data.Kraken.OpenOrderList parsing OpenOrder" (fmap V.toList . mapM parseJSON)) (mkElems o)
      return $ OpenOrderList $ zipWith OpenOrderObject (mkKeys o) datas
    where
      mkKeys o = filter (/= "last") (HM.keys o)
      mkElems o = HM.elems $ HM.filterWithKey (\k _ -> k /= "last") o

prettyOpenOrderList :: OpenOrderList -> Doc
prettyOpenOrderList (OpenOrderList xs) = colName "Open Orders" $$ nest n2 (prettyOpenOrderObject lst)
  where
    n2 = nestCols - nesting
