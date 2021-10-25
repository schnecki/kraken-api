{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
module Data.Kraken.OpenOrder
    ( OpenOrder (..)
    , prettyOpenOrder
    , prettyOpenOrderWith
    ) where


import           Control.DeepSeq
import           Control.Monad                (unless)
import           Data.Aeson
import           Data.Kraken.OrderDescription
import           Data.Kraken.OrderFlags
import           Data.Kraken.OrderMinimum
import           Data.Kraken.OrderMisc
import           Data.Kraken.OrderStatus
import           Data.Serialize
import           Data.Serialize.Text          ()
import qualified Data.Text                    as T
import qualified Data.Vector                  as V
import           GHC.Generics
import           Text.PrettyPrint

import           Data.Kraken.DateTime
import           Data.Kraken.PriceValue
import           Data.Kraken.Util

data OpenOrder =
  OpenOrder
    { refid      :: Maybe T.Text     -- ^ Referral order transaction ID that created this order
    , userref    :: T.Text           -- ^ User reference id
    , status     :: OrderStatus      -- ^ Status of order
    , opentm     :: DateTime         -- ^ Unix timestamp of when order was placed
    , starttm    :: DateTime         -- ^ Unix timestamp of order start time (or 0 if not set)
    , expiretm   :: DateTime         -- ^ Unix timestamp of order end time (or 0 if not set)
    , descr      :: OrderDescription -- ^ Order description info
    , vol        :: Double           -- ^ Volume of order (base currency)
    , volExec    :: Double           -- ^ Volume executed (base currency)
    , cost       :: PriceValue       -- ^ Total cost (quote currency unless)
    , fee        :: PriceValue       -- ^ Total fee (quote currency)
    , price      :: PriceValue       -- ^ Average price (quote currency)
    , stopprice  :: PriceValue       -- ^ Stop price (quote currency)
    , limitprice :: PriceValue       -- ^ Triggered limit price (quote currency, when limit based order type triggered)
    , misc       :: [OrderMisc]      -- ^ Comma delimited list of miscellaneous info
    , oflags     :: [OrderFlags]     -- ^ Comma delimited list of order flags
    , trades     :: [T.Text]         -- ^ List of trade IDs related to order (if trades info requested and data available)
    }
  deriving (Read, Show, Eq, Ord, Generic, NFData, ToJSON, Serialize)


instance FromJSON OpenOrder where
  parseJSON =
    withArray "Data.Kraken.OpenOrder" $ \arr -> do
      unless (V.length arr == 17) $ fail ("Expected an array of length 17, but encountered: " ++ show arr)
      ref <- parseJSON (arr V.! 0)
      use <- parseJSON (arr V.! 1)
      stat<- parseJSON (arr V.! 2)
      ope <- parseJSON (arr V.! 3)
      star<- parseJSON (arr V.! 4)
      exp <- parseJSON (arr V.! 5)
      des <- parseJSON (arr V.! 6)
      vol <- parseJSON (arr V.! 7)
      volE<- parseJSON (arr V.! 8)
      cos <- parseJSON (arr V.! 9)
      fee <- parseJSON (arr V.! 10)
      pri <- parseJSON (arr V.! 11)
      sto <- parseJSON (arr V.! 12)
      lim <- parseJSON (arr V.! 13)
      mis <- mapM parseJSON . (map String . T.splitOn ",") =<< parseJSON (arr V.! 14)
      ofl <- mapM parseJSON . (map String . T.splitOn ",") =<< parseJSON (arr V.! 15)
      tra <- mapM parseJSON . (map String . T.splitOn ",") =<< parseJSON (arr V.! 16)
      return $ OpenOrder ref use stat ope star exp des vol volE cos fee pri sto lim mis ofl tra


prettyOpenOrder :: OpenOrder -> Doc
prettyOpenOrder = prettyOpenOrderWith 0

prettyOpenOrderWith :: Int -> OpenOrder -> Doc
prettyOpenOrderWith nesting ord =
  mVal (refid ord) (\v -> colName "refid"      $$ nest n2 (prettyText $ v)) $+$
  colName "userref"    $$ nest n2 (prettyText $ userref ord) $+$
  colName "status"     $$ nest n2 (prettyOrderStatus $ status ord) $+$
  colName "opentm"     $$ nest n2 (prettyDateTime $ opentm ord) $+$
  colName "starttm"    $$ nest n2 (prettyDateTime $ starttm ord) $+$
  colName "expiretm"   $$ nest n2 (prettyDateTime $ expiretm ord) $+$
  colName "descr"      $$ nest n2 (prettyOrderDescription $ descr ord) $+$
  colName "vol"        $$ nest n2 (prettyDouble $ vol ord) $+$
  colName "volExec"    $$ nest n2 (prettyDouble $ volExec ord) $+$
  colName "cost"       $$ nest n2 (prettyPriceValue $ cost ord) $+$
  colName "fee"        $$ nest n2 (prettyPriceValue $ fee ord) $+$
  colName "price"      $$ nest n2 (prettyPriceValue $ Data.Kraken.OpenOrder.price ord) $+$
  colName "stopprice"  $$ nest n2 (prettyPriceValue $ stopprice ord) $+$
  colName "limitprice" $$ nest n2 (prettyPriceValue $ limitprice ord) $+$
  colName "misc"       $$ nest n2 (hsep $ punctuate (text ", ") $ map prettyOrderMisc $ misc ord) $+$
  colName "oflags"     $$ nest n2 (hsep $ punctuate (text ", ") $ map prettyOrderFlags $ oflags ord) $+$
  colName "trades"     $$ nest n2 (hsep $ punctuate (text ", ") $ map prettyText $ trades ord)
  where n2 = nestCols - nesting
