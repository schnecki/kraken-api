{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
module Data.Kraken.CandlestickGranularity
    ( CandlestickGranularity (..)
    , candlestickGranularityInSeconds
    , candlestickGranularityInMinutes
    , candlestickGranularityToNomialDiffTime
    ) where

import           Control.DeepSeq
import           Data.Aeson
import           Data.Serialize
import           Data.Time.Clock
import           GHC.Generics    hiding (M1)

-- | This represents the intervals in minutes: 1 5 15 30 60 240 1440 10080 21600
data CandlestickGranularity
  = M1   -- ^ 1 minute candlesticks, minute alignment.
  | M5   -- ^ 5 minute candlesticks, hour alignment
  | M15  -- ^ 15 minute candlesticks, hour alignment
  | M30  -- ^ 30 minute candlesticks, hour alignment
  | H1   -- ^ 1 hour candlesticks, hour alignment. Interval: 60
  | H4   -- ^ 4 hour candlesticks, day alignment. Interval: 240
  | D1   -- ^ 1 day candlesticks, day alignment. Interval: 1440
  | D7   -- ^ 1 week candlesticks, aligned to start of week: Interval: 10080
  | D15  -- ^ 15 days candlesticks, aligned to first day of the month. Interval 21600
  deriving (Show, Read, Eq, Ord, Bounded, Enum, Serialize, Generic, ToJSON, FromJSON, NFData)


candlestickGranularityInSeconds :: CandlestickGranularity -> Int
candlestickGranularityInSeconds M1  = 60
candlestickGranularityInSeconds M5  = 300
candlestickGranularityInSeconds M15 = 900
candlestickGranularityInSeconds M30 = 1800
candlestickGranularityInSeconds H1  = 3600
candlestickGranularityInSeconds H4  = 14400
candlestickGranularityInSeconds D1  = 86400
candlestickGranularityInSeconds D7  = 7*86400
candlestickGranularityInSeconds D15 = 15*86400

candlestickGranularityInMinutes :: CandlestickGranularity -> Int
candlestickGranularityInMinutes x = candlestickGranularityInSeconds x `div` 60


candlestickGranularityToNomialDiffTime :: CandlestickGranularity -> NominalDiffTime
candlestickGranularityToNomialDiffTime = fromRational . toRational . candlestickGranularityInSeconds
