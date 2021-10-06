{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
module Data.Kraken.TickerInformation
    ( TickerInformation (..)
    , prettyTickerInformation
    , prettyTickerInformationWith
    ) where


import           Control.DeepSeq
import           Data.Aeson
import           Data.Serialize
import           Data.Serialize.Text          ()
import           GHC.Generics
import           Text.PrettyPrint

import           Data.Kraken.Candlestick
import           Data.Kraken.CandlestickStats
import           Data.Kraken.PriceValue
import           Data.Kraken.TradeCount
import           Data.Kraken.TradeVolume
import           Data.Kraken.Util


data TickerInformation =
  TickerInformation
    { ask            :: Candlestick      -- ^ Ask [<price>, <whole lot volume>, <lot volume>]
    , bid            :: Candlestick      -- ^ Bid [<price>, <whole lot volume>, <lot volume>]
    , close          :: Candlestick      -- ^ Last trade closed [<price>, <lot volume>]
    , volume         :: TradeVolume      -- ^ Volume [<today>, <last 24 hours>]
    , volumeWeigthed :: TradeVolume      -- ^ Volume weighted average price [<today>, <last 24 hours>]
    , tradeCount     :: TradeCount       -- ^ Number of trades [<today>, <last 24 hours>]
    , low            :: CandlestickStats -- ^ Low [<today>, <last 24 hours>]
    , high           :: CandlestickStats -- ^ High [<today>, <last 24 hours>]
    , open           :: PriceValue       -- ^ Today's opening price
    }
  deriving (Read, Show, Eq, Ord, Generic, NFData, Serialize)


instance FromJSON TickerInformation where
  parseJSON = withObject "Object of Type Data.Kraken.TickerInformation" $ \o -> do
    a <- o .: "a"
    b <- o .: "b"
    c <- o .: "c"
    v <- o .: "v"
    p <- o .: "p"
    t <- o .: "t"
    l <- o .: "l"
    h <- o .: "h"
    op <- o .: "o"
    return $ TickerInformation a b c v p t l h op

prettyTickerInformation :: TickerInformation -> Doc
prettyTickerInformation = prettyTickerInformationWith 0

prettyTickerInformationWith :: Int -> TickerInformation -> Doc
prettyTickerInformationWith nesting info =
  colName "ask"            $$ nest n1 (prettyCandlestickWith n1Sum $ ask info) $+$
  colName "bid"            $$ nest n1 (prettyCandlestickWith n1Sum $ bid info) $+$
  colName "close"          $$ nest n1 (prettyCandlestickWith n1Sum $ close info) $+$
  colName "volume"         $$ nest n1 (prettyTradeVolumeWith n1Sum $ volume info) $+$
  colName "volumeWeigthed" $$ nest n1 (prettyTradeVolumeWith n1Sum $ volumeWeigthed info) $+$
  colName "tradeCount"     $$ nest n1 (prettyTradeCountWith  n1Sum $ tradeCount info) $+$
  colName "low"            $$ nest n1 (prettyCandlestickStatsWith n1Sum $ low info) $+$
  colName "high"           $$ nest n1 (prettyCandlestickStatsWith n1Sum $ high info) $+$
  colName "open"           $$ nest n2 (prettyPriceValue $ open info)
  where n1 = 2 * nestIndent
        n1Sum = nesting + n1
        n2 = nestCols - nesting


-- data TickerInfo' =
--   TickerInfo'
--     { a :: [T.Text] -- ^ Ask [<price>, <whole lot volume>, <lot volume>]
--     , b :: [T.Text] -- ^ Bid [<price>, <whole lot volume>, <lot volume>]
--     , c :: [T.Text] -- ^ Last trade closed [<price>, <lot volume>]
--     , v :: [T.Text] -- ^ Volume [<today>, <last 24 hours>]
--     , p :: [T.Text] -- ^ Volume weighted average price [<today>, <last 24 hours>]
--     , t :: [Int]    -- ^ Number of trades [<today>, <last 24 hours>]
--     , l :: [T.Text] -- ^ Low [<today>, <last 24 hours>]
--     , h :: [T.Text] -- ^ High [<today>, <last 24 hours>]
--     , o :: T.Text   -- ^ Today's opening price
--     }
--   deriving (Read, Show, Eq, Ord, Generic, NFData, Serialize)


-- toTickerInfo' :: TickerInfo -> TickerInfo'
-- toTickerInfo' = undefined


-- fromTickerInfo' :: TickerInfo' -> TickerInfo
-- fromTickerInfo' = undefined


-- instance ToJSON TickerInformation where
--    toJSON = genericToJSON jsonSnakeCase
