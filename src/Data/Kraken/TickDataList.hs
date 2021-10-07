{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
module Data.Kraken.TickDataList
    ( TickDataList (..)
    , setTickDataListGranularity
    , prettyTickDataList
    ) where


import           Control.DeepSeq
import           Data.Aeson
import qualified Data.HashMap.Strict                as HM (elems, filterWithKey, keys)
import           Data.Serialize
import qualified Data.Vector                        as V
import           GHC.Generics
import           Text.PrettyPrint

import           Data.Kraken.CandlestickGranularity
import           Data.Kraken.DateTime
import           Data.Kraken.TickDataObject
import           Data.Kraken.Util


data TickDataList =
  TickDataList
    { tickGranularity :: Maybe CandlestickGranularity
    , tickLast        :: DateTime
    , tickDatas       :: [TickDataObject]
    }
  deriving (Read, Show, Eq, Ord, Generic, NFData, Serialize)


instance FromJSON TickDataList where
  parseJSON =
    withObject "Data.Kraken.TickDataList" $ \o -> do
      last' <- secondsToDateTime <$> o .: "last"
      datas <- mapM (withArray "Data.Kraken.TickDataList parsing TickData" $ \o' -> V.toList <$> mapM parseJSON o') (mkElems o)
      return $ TickDataList Nothing last' $ zipWith TickDataObject (mkKeys o) datas
    where
      mkKeys o = filter (/= "last") (HM.keys o)
      mkElems o = HM.elems $ HM.filterWithKey (\k _ -> k /= "last") o

setTickDataListGranularity :: CandlestickGranularity -> TickDataList -> TickDataList
setTickDataListGranularity g x = x {tickGranularity = Just g}


prettyTickDataList :: TickDataList -> Doc
prettyTickDataList (TickDataList mGran lst xs) =
  mVal mGran (\v -> colName "Tick Granularity" $$ nest nestCols (text $ show v)) $+$
  colName "Tick Last Date" $$ nest nestCols (prettyDateTime lst) $+$
  vcat (map prettyTickDataObject xs)
