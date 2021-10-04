{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
module Data.Kraken.SystemStatusType
    ( SystemStatusType (..)

    ) where

import           Control.DeepSeq
import           Data.Aeson
import           Data.Serialize
import           GHC.Generics

import           Data.Kraken.Util

data SystemStatusType
  = Online      -- ^ Kraken is operating normally. All order types may be submitted and trades can occur.
  | Maintenance -- ^ The exchange is offline. No new orders or cancellations may be submitted.
  | CancelOnly -- ^ Resting (open) orders can be cancelled but no new orders may be submitted. No trades will occur.
  | PostOnly   -- ^ Only post-only limit orders can be submitted. Existing orders may still be cancelled. No trades will occur.
  deriving (Enum, Bounded, Read, Show, Eq, Ord, Generic, NFData, Serialize)


instance ToJSON SystemStatusType where
   toJSON = genericToJSON jsonSnakeCase

instance FromJSON SystemStatusType where
  parseJSON = genericParseJSON jsonSnakeCase
