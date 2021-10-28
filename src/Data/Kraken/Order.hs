{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
module Data.Kraken.Order
    ( Order (..)
    , prettyOrder
    , prettyOrderWith
    ) where


import           Control.DeepSeq
import           Data.Aeson
import           Data.Kraken.OrderDescription
import           Data.Kraken.OrderFlags
import           Data.Kraken.OrderMisc
import           Data.Kraken.OrderStatus
import           Data.Serialize
import           Data.Serialize.Text          ()
import qualified Data.Text                    as T
import           GHC.Generics
import           Text.PrettyPrint

import           Data.Kraken.DateTime
import           Data.Kraken.PriceValue
import           Data.Kraken.Util


data Order =
  Order
    { refid      :: Maybe T.Text     -- ^ Referral order transaction ID that created this order
    , userref    :: Maybe T.Text     -- ^ User reference id
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
    , reason     :: Maybe T.Text     -- ^ Additional info on status (if any)
    }
  deriving (Read, Show, Eq, Ord, Generic, NFData, ToJSON, Serialize)


instance FromJSON Order where
  parseJSON =
    withObject "Data.Kraken.Order" $ \o -> do
      ref <- o .: "refid"
      use <- o .: "userref"
      stat <- o .: "status"
      ope <- unixTimeStampToDateTime <$> o .: "opentm"
      star <- unixTimeStampToDateTime <$> o .: "starttm"
      exp <- unixTimeStampToDateTime <$> o .: "expiretm"
      des <- o .: "descr"
      vol <- parseStrToDouble =<< o .: "vol"
      volE <- parseStrToDouble =<< o .: "vol_exec"
      cos <- o .: "cost"
      fee <- o .: "fee"
      pri <- o .: "price"
      sto <- o .: "stopprice"
      lim <- o .: "limitprice"
      mis <- mapM parseJSON . (map String . filter (not . T.null) . T.splitOn ",") =<< o .: "misc"
      ofl <- mapM parseJSON . (map String . filter (not . T.null) . T.splitOn ",") =<< o .: "oflags"
      tra <- maybe (return []) parseList =<< o .:? "trades"
      rea <- o .: "reason"
      return $ Order ref use stat ope star exp des vol volE cos fee pri sto lim mis ofl tra rea
    where parseList = mapM parseJSON . (map String . filter (not . T.null) . T.splitOn ",")


prettyOrder :: Order -> Doc
prettyOrder = prettyOrderWith 0

prettyOrderWith :: Int -> Order -> Doc
prettyOrderWith nesting ord =
  mVal (refid ord) (\v -> colName "refid"      $$ nest n2 (prettyText v)) $+$
  mVal (userref ord) (\v -> colName "userref"    $$ nest n2 (prettyText v)) $+$
  colName "status"     $$ nest n2 (prettyOrderStatus $ status ord) $+$
  colName "opentm"     $$ nest n2 (prettyDateTime $ opentm ord) $+$
  colName "starttm"    $$ nest n2 (prettyDateTime $ starttm ord) $+$
  colName "expiretm"   $$ nest n2 (prettyDateTime $ expiretm ord) $+$
  colName "descr"      $$ nest nestIndent (prettyOrderDescriptionWith (nesting + nestIndent) $ descr ord) $+$
  colName "vol"        $$ nest n2 (prettyDouble $ vol ord) $+$
  colName "volExec"    $$ nest n2 (prettyDouble $ volExec ord) $+$
  colName "cost"       $$ nest n2 (prettyPriceValue $ cost ord) $+$
  colName "fee"        $$ nest n2 (prettyPriceValue $ fee ord) $+$
  colName "price"      $$ nest n2 (prettyPriceValue $ Data.Kraken.Order.price ord) $+$
  colName "stopprice"  $$ nest n2 (prettyPriceValue $ stopprice ord) $+$
  colName "limitprice" $$ nest n2 (prettyPriceValue $ limitprice ord) $+$
  colName "misc"       $$ nest n2 (hsep $ punctuate (text ", ") $ map prettyOrderMisc $ misc ord) $+$
  colName "oflags"     $$ nest n2 (hsep $ punctuate (text ", ") $ map prettyOrderFlags $ oflags ord) $+$
  colName "trades"     $$ nest n2 (hsep $ punctuate (text ", ") $ map prettyText $ trades ord)
  where n2 = nestCols - nesting
