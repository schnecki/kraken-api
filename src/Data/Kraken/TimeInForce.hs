{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}


module Data.Kraken.TimeInForce
    ( TimeInForce (..)

    ) where

import           Control.DeepSeq
import           Data.Aeson
import           GHC.Generics


data TimeInForce
  = GTC -- ^ The Order is “Good unTil Cancelled”
  | GTD -- ^ The Order is “Good unTil Date” and will be cancelled at the provided time
  | IOC -- ^ The Order must be “Immediatedly paritally filled Or Cancelled”
  deriving (Bounded, Enum, Show, Read, Eq, Ord, FromJSON, ToJSON, Generic, NFData)
