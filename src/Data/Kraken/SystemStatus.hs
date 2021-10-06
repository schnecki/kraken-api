{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
module Data.Kraken.SystemStatus
    ( SystemStatus (..)
    , prettySystemStatus
    ) where

import           Control.DeepSeq
import           Data.Aeson
import           Data.Serialize
import           Data.Serialize.Text          ()
import           GHC.Generics
import           Text.PrettyPrint

import           Data.Kraken.DateTime
import           Data.Kraken.SystemStatusType
import           Data.Kraken.Util

data SystemStatus = SystemStatus
  { status    :: SystemStatusType         -- ^ The system status
  , timestamp :: DateTime
  } deriving (Show, Read, Eq, Ord, Serialize, Generic, FromJSON, NFData)


prettySystemStatus :: SystemStatus -> Doc
prettySystemStatus st =
  colName "System Status"      $$ nest nestCols (text $ show $ status st) $+$
  colName "System Timestamp"       $$ nest nestCols (text $ show $ timestamp st)

