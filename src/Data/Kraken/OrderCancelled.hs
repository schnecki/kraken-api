{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
module Data.Kraken.OrderCancelled
    ( OrderCancelled (..)
    , prettyOrderCancelled
    , prettyOrderCancelledWith
    ) where

import           Control.DeepSeq
import           Data.Aeson
import           Data.Serialize
import           Data.Serialize.Text ()
import qualified Data.Text           as T
import           GHC.Generics
import           Text.PrettyPrint

import           Data.Kraken.Util


data OrderCancelled =
  OrderCancelled
    { count   :: Int
    , pending :: Maybe Bool
    }
  deriving (Read, Show, Eq, Ord, Generic, NFData, ToJSON, FromJSON, Serialize)


prettyOrderCancelled :: OrderCancelled -> Doc
prettyOrderCancelled = prettyOrderCancelledWith 0

prettyOrderCancelledWith :: Int -> OrderCancelled -> Doc
prettyOrderCancelledWith nesting ord =
  colName "count"                             $$ nest n2 (int $ count ord) $+$
  mVal (pending ord) (\v -> colName "pending" $$ nest n2 (prettyText $ T.pack $ show v))
  where n2 = nestCols - nesting
