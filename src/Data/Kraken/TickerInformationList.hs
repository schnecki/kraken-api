{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
module Data.Kraken.TickerInformationList
    ( TickerInformationList(..)
    , prettyTickerInformationList
    ) where

import           Control.DeepSeq
import           Data.Aeson
import           Data.Aeson.KeyMap                   (toHashMapText)
import           Data.HashMap.Strict                 (elems, keys)
import           Data.List                           (intersperse)
import           Data.Serialize
import           GHC.Generics
import           Text.PrettyPrint

import           Data.Kraken.TickerInformationObject


data TickerInformationList =
  TickerInformationList
    { tickerInfos :: [TickerInformationObject]
    }
  deriving (Read, Show, Eq, Ord, Generic, NFData, Serialize)


instance FromJSON TickerInformationList where
  parseJSON =
    withObject "Data.Kraken.TickerInformationList" $ \o ->
      let oHM = toHashMapText o
       in TickerInformationList . zipWith TickerInformationObject (keys oHM) <$> mapM parseJSON (elems oHM)


prettyTickerInformationList :: TickerInformationList -> Doc
prettyTickerInformationList (TickerInformationList xs) = vcat (map prettyTickerInformationObject xs)
