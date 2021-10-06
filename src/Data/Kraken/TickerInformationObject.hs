{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
module Data.Kraken.TickerInformationObject
    ( TickerInformationObject (..)
    , prettyTickerInformationObject
    ) where


import           Control.DeepSeq
import           Data.Serialize
import           Data.Serialize.Text           ()
import qualified Data.Text                     as T
import           GHC.Generics
import           Text.PrettyPrint

import           Data.Kraken.TickerInformation
import           Data.Kraken.Types
import           Data.Kraken.Util


data TickerInformationObject = TickerInformationObject
  { tickerInfoPairName :: InstrumentName
  , tickerInformation  :: TickerInformation
  }
  deriving (Read, Show, Eq, Ord, Generic, NFData, Serialize)


prettyTickerInformationObject :: TickerInformationObject -> Doc
prettyTickerInformationObject (TickerInformationObject n p) =
  colName "Ticker Information Pair" $$ nest nestCols (text $ T.unpack n) $+$
  -- colName "Ticker Information"      $+$
  nest nestIndent (prettyTickerInformationWith nestIndent p)


