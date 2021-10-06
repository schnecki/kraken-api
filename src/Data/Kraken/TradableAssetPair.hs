{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
module Data.Kraken.TradableAssetPair
    ( TradableAssetPair(..)
    , prettyTradableAssetPair
    ) where

import           Control.Applicative    ((<|>))
import           Control.DeepSeq
import           Data.Aeson
import qualified Data.HashMap.Strict    as HM
import           Data.Serialize
import           Data.Serialize.Text    ()
import qualified Data.Text              as T
import           GHC.Generics
import           Prelude                hiding ((<>))
import           Text.PrettyPrint

import           Data.Kraken.AssetClass
import           Data.Kraken.Types
import           Data.Kraken.Util

import           Debug.Trace


data TradableAssetPair
  = TradableAssetPair
      { altname           :: InstrumentName       -- ^ Alternate pair name
      , wsname            :: Maybe InstrumentName -- ^ WebSocket pair name (if available)
      , aclassBase        :: AssetClass           -- ^ Asset class of base component
      , base              :: InstrumentName       -- ^ Asset ID of base component
      , aclassQuote       :: AssetClass           -- ^ Asset class of quote component
      , quote             :: InstrumentName       -- ^ Asset ID of quote component
      , lot               :: T.Text               -- ^ deprecated Volume lot size -- ^
      , airDecimals       :: Integer              -- ^ caling decimal places for pair
      , lotDecimals       :: Integer              -- ^ Scaling decimal places for volume
      , lotMultiplier     :: Integer              -- ^ Amount to multiply lot volume by to get currency volume
      , leverageBuy       :: [Integer]            -- ^ Array of leverage amounts available when buying
      , leverageSell      :: [Integer]            -- ^ Array of leverage amounts available when selling
      , fees              :: [[Double]]           -- ^ Fee schedule array in [<volume>, <percent fee>] tuples
      , feesMaker         :: [[Double]]           -- ^  Maker fee schedule array in [<volume>, <percent fee>] tuples (if on maker/taker)
      , feeVolumeCurrency :: InstrumentName       -- ^ Volume discount currency
      , marginCall        :: Integer              -- ^ Margin call level
      , marginStop        :: Integer              -- ^ Stop-out/liquidation margin level
      , ordermin          :: Double               -- ^ Minimum order size (in terms of base currency)
      }
  | TradableAssetPairLeverage
      { leverageBuy  :: [Integer] -- ^ Array of leverage amounts available when buying
      , leverageSell :: [Integer] -- ^ Array of leverage amounts available when selling
      }
  | TradableAssetPairFees
      { fees      :: [[Double]] -- ^ Fee schedule array in [<volume>, <percent fee>] tuples
      , feesMaker :: [[Double]] -- ^  Maker fee schedule array in [<volume>, <percent fee>] tuples (if on maker/taker)
      }
  | TradableAssetPairMargin
      { marginCall  :: Integer -- ^ Margin call level
      , marginLevel :: Integer -- ^ Stop-out/liquidation margin level
      }
  deriving (Read, Show, Eq, Ord, Generic, NFData, Serialize)


instance ToJSON TradableAssetPair where
   toJSON = genericToJSON jsonSnakeCase


instance FromJSON TradableAssetPair where
  parseJSON (Object v) =
    genericParseJSON jsonSnakeCase (objTag "tradable_asset_pair") <|>
    genericParseJSON jsonSnakeCase (objTag "tradable_asset_pair_leverage") <|>
    genericParseJSON jsonSnakeCase (objTag "tradable_asset_pair_fees") <|>
    genericParseJSON jsonSnakeCase (objTag "tradable_asset_pair_margin")
    where objTag tag = Object $ HM.insert "tag" (String tag) v
  parseJSON x = fail $ "Unexpected type in FromJSON of Data.Kraken.TradableAssetPair: " ++ show x


prettyTradableAssetPair :: TradableAssetPair -> Doc
prettyTradableAssetPair pair =
    colName "altname"            $$ nest nestCols (text . T.unpack $ altname pair) $+$
    colName "wsname"             $$ nest nestCols (maybe mempty (text . T.unpack) $ wsname pair) $+$
    colName "aclassBase"         $$ nest nestCols (prettyAssetClass $ aclassBase pair) $+$
    colName "base"               $$ nest nestCols (text . T.unpack $ base pair) $+$
    colName "aclassQuote"        $$ nest nestCols (prettyAssetClass $ aclassQuote pair) $+$
    colName "quote"              $$ nest nestCols (text . T.unpack $ quote pair) $+$
    colName "lot"                $$ nest nestCols (text . T.unpack $ lot pair) $+$
    colName "airDecimals"        $$ nest nestCols (integer $ airDecimals pair) $+$
    colName "lotDecimals"        $$ nest nestCols (integer $ lotDecimals pair) $+$
    colName "lotMultiplier"      $$ nest nestCols (integer $ lotMultiplier pair) $+$
    colName "leverageBuy"        $$ nest nestCols (hsepComma $ map integer $ leverageBuy pair) $+$
    colName "leverageSell"       $$ nest nestCols (hcat $ map integer $ leverageSell pair) $+$
    colName "fees"               $$ nest nestCols (hsepComma $ map tuple $ fees pair) $+$
    colName "feesMaker"          $$ nest nestCols (hsepComma $ map tuple $ feesMaker pair) $+$
    colName "feeVolumeCurrency"  $$ nest nestCols (text . T.unpack $ feeVolumeCurrency pair) $+$
    colName "marginCall"         $$ nest nestCols (integer $ marginCall pair) $+$
    colName "marginStop"         $$ nest nestCols (integer $ marginStop pair) $+$
    colName "ordermin"           $$ nest nestCols (double $ ordermin pair)
  where tuple [a, b] = parens (double a <> comma <+> double b)
        tuple x      = text (show x)
