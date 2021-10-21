{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
module Data.Kraken.AccountBalance
    ( AccountBalance (..)
    , prettyAccountBalance
    , prettyAccountBalanceWith
    ) where


import           Control.DeepSeq
import           Data.Aeson
import           Data.Serialize
import           Data.Serialize.Text      ()
import qualified Data.Text                as T
import           GHC.Generics
import           Text.PrettyPrint

import           Data.Kraken.AccountUnits
import           Data.Kraken.Types
import           Data.Kraken.Util


data AccountBalance =
  AccountBalance
    { instrument :: InstrumentName
    , balance    :: AccountUnits
    }
  deriving (Read, Show, Eq, Ord, Generic, NFData, Serialize)


instance ToJSON AccountBalance where
   toJSON = genericToJSON jsonSnakeCase

instance FromJSON AccountBalance where
  parseJSON = genericParseJSON jsonSnakeCase


prettyAccountBalance :: AccountBalance -> Doc
prettyAccountBalance = prettyAccountBalanceWith 0

prettyAccountBalanceWith :: Int -> AccountBalance -> Doc
prettyAccountBalanceWith nesting info =
  colName "Instrument"      $$ nest n2 (text $ T.unpack $ instrument info) $+$
  colName "Balance"         $$ nest n2 (prettyAccountUnits $ balance info)
  where n2 = nestCols - nesting
