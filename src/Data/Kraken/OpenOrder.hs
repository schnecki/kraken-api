{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
module Data.Kraken.OpenOrder
    ( OpenOrder (..)
    , prettyOpenOrder
    , prettyOpenOrderWith
    ) where


import           Control.DeepSeq
import           Control.Monad            (unless)
import           Data.Aeson
-- import           Data.Kraken.OrderDescription
import           Data.Kraken.OrderFlags
import           Data.Kraken.OrderMinimum
import           Data.Kraken.OrderMisc
import           Data.Kraken.OrderStatus
import           Data.Serialize
import           Data.Serialize.Text      ()
import qualified Data.Text                as T
import qualified Data.Vector              as V
import           GHC.Generics
import           Text.PrettyPrint

import           Data.Kraken.DateTime
import           Data.Kraken.PriceValue
import           Data.Kraken.Util

data OpenOrder =
  OpenOrder
    { refid      :: Maybe T.Text     -- ^ Referral order transaction ID that created this order
    , userref    :: T.Text         -- ^ User reference id
    , status     :: OrderStatus     -- ^ Status of order
    , opentm     :: DateTime        -- ^ Unix timestamp of when order was placed
    , starttm    :: DateTime       -- ^ Unix timestamp of order start time (or 0 if not set)
    , expiretm   :: DateTime      -- ^ Unix timestamp of order end time (or 0 if not set)
    , descr      :: OrderDescription -- ^ Order description info
    , vol        :: Double             -- ^ Volume of order (base currency)
    , vol_exec   :: Double        -- ^ Volume executed (base currency)
    , cost       :: PriceValue        -- ^ Total cost (quote currency unless)
    , fee        :: PriceValue         -- ^ Total fee (quote currency)
    , price      :: PriceValue       -- ^ Average price (quote currency)
    , stopprice  :: PriceValue   -- ^ Stop price (quote currency)
    , limitprice :: PriceValue  -- ^ Triggered limit price (quote currency, when limit based order type triggered)
    , misc       :: [OrderMisc] -- ^ Comma delimited list of miscellaneous info
    , oflags     :: [OrderFlags]    -- ^ Comma delimited list of order flags
    , trades     :: [T.Text]        -- ^ List of trade IDs related to order (if trades info requested and data available)
    }
  deriving (Read, Show, Eq, Ord, Generic, NFData, ToJSON, Serialize)


instance FromJSON OpenOrder where
  parseJSON =
    withArray "Data.Kraken.OpenOrder" $ \arr -> do
      unless (V.length arr == 8) $ fail ("Expected an array of length 8, but encountered: " ++ show arr)
      ti <- secondsToDateTime <$> parseJSON (arr V.! 0)
      op <- parseJSON (arr V.! 1)
      hi <- parseJSON (arr V.! 2)
      lo <- parseJSON (arr V.! 3)
      cl <- parseJSON (arr V.! 4)
      vw <- parseJSON (arr V.! 5)
      vo <- parseJSON =<< parseStrToNum (arr V.! 6)
      co <- parseJSON (arr V.! 7)
      return $ OpenOrder ti op hi lo cl vw vo co


prettyOpenOrder :: OpenOrder -> Doc
prettyOpenOrder = prettyOpenOrderWith 0

prettyOpenOrderWith :: Int -> OpenOrder -> Doc
prettyOpenOrderWith nesting info =
  colName "time"    $$ nest n2 (prettyDateTime $ time info)  $+$
  colName "open"    $$ nest n2 (prettyPriceValue $ open info)  $+$
  colName "high"    $$ nest n2 (prettyPriceValue $ high info)  $+$
  colName "low"     $$ nest n2 (prettyPriceValue $ low info)  $+$
  colName "close"   $$ nest n2 (prettyPriceValue $ close info)  $+$
  colName "vwap"    $$ nest n2 (prettyPriceValue $ vwap info)  $+$
  colName "volume"  $$ nest n2 (double $ volume info)  $+$
  colName "count"   $$ nest n2 (int $ count info)
  where n2 = nestCols - nesting
