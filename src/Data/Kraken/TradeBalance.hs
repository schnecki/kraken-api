{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
module Data.Kraken.TradeBalance
    ( TradeBalance (..)
    , prettyTradeBalance
    , prettyTradeBalanceWith
    ) where


import           Control.DeepSeq
import           Data.Aeson
import           Data.Aeson.Types
import           Data.Serialize
import           Data.Serialize.Text    ()
import qualified Data.Text              as T
import           GHC.Generics
import           Text.PrettyPrint

import           Data.Kraken.PriceValue
import           Data.Kraken.Types
import           Data.Kraken.Util

data TradeBalance =
  TradeBalance
    { baseAsset    :: Maybe InstrumentName -- ^ Base asset used to determine balance
    , equivBalance :: PriceValue           -- ^ Equivalent balance (combined balance of all currencies)
    , tradeBalance :: PriceValue           -- ^ Trade balance (combined balance of all equity currencies)
    , margin       :: PriceValue           -- ^ Margin amount of open positions
    , netProfit    :: PriceValue           -- ^ Unrealized net profit/loss of open positions
    , costBasis    :: PriceValue           -- ^ Cost basis of open positions
    , valuation    :: PriceValue           -- ^ Current floating valuation of open positions
    , equity       :: PriceValue           -- ^ Equity: trade balance + unrealized net profit/loss
    , marginFree   :: PriceValue           -- ^ Free margin: Equity - initial margin (maximum margin available to open new positions)
    , marginLevel  :: Maybe PriceValue     -- ^ Margin level: (equity / initial margin) * 100
    }
  deriving (Read, Show, Eq, Ord, Generic, NFData, Serialize)


instance ToJSON TradeBalance where
   toJSON = genericToJSON jsonSnakeCase

instance FromJSON TradeBalance where
  parseJSON (Object o) = TradeBalance Nothing <$> o .: "eb" <*> o .: "tb" <*> o .: "m" <*> o .: "n" <*> o .: "c" <*> o .: "v" <*> o .: "e" <*> o .: "mf" <*> o .:? "ml"
  parseJSON invalid    = prependFailure "parsing Data.Kraken.TradeBalance failed, " (typeMismatch "Object" invalid)

prettyTradeBalance :: TradeBalance -> Doc
prettyTradeBalance = prettyTradeBalanceWith 0

prettyTradeBalanceWith :: Int -> TradeBalance -> Doc
prettyTradeBalanceWith nesting bal =
    mVal (baseAsset bal) (\base -> colName "BaseAsset"  $$ nest n2 (text $ T.unpack base)) $+$
    colName "EquivBalance"                              $$ nest n2 (prettyPriceValue $ equivBalance bal)  $+$
    colName "TradeBalance"                              $$ nest n2 (prettyPriceValue $ tradeBalance bal)  $+$
    colName "Margin"                                    $$ nest n2 (prettyPriceValue $ margin bal) $+$
    colName "NetProfit"                                 $$ nest n2 (prettyPriceValue $ netProfit bal) $+$
    colName "CostBasis"                                 $$ nest n2 (prettyPriceValue $ costBasis bal) $+$
    colName "Valuation"                                 $$ nest n2 (prettyPriceValue $ valuation bal) $+$
    colName "Equity"                                    $$ nest n2 (prettyPriceValue $ equity bal) $+$
    colName "MarginFree"                                $$ nest n2 (prettyPriceValue $ marginFree bal) $+$
    mVal (marginLevel bal) (\v -> colName "MarginLevel" $$ nest n2 (prettyPriceValue v))
  where n2 = nestCols - nesting
