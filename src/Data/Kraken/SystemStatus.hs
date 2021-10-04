{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
module Data.Kraken.SystemStatus
    ( SystemStatus (..)
    , prettySystemStatus
    ) where

import           Control.DeepSeq
import           Data.Aeson
import qualified Data.ByteString.Lazy         as BS
import           Data.Serialize
import           Data.Serialize.Text          ()
import qualified Data.Text                    as T
import           GHC.Generics
import           Text.PrettyPrint

import           Data.Kraken.DateTime
import           Data.Kraken.RequestResult
import           Data.Kraken.SystemStatusType
import           Data.Kraken.Util

data SystemStatus = SystemStatus
  { status    :: SystemStatusType         -- ^ The system status
  , timestamp :: DateTime
  } deriving (Show, Read, Eq, Ord, Serialize, Generic, FromJSON, NFData)


responseBody :: BS.ByteString
responseBody = "{\"error\":[],\"result\":{\"status\":\"online\",\"timestamp\":\"2021-10-04T18:41:07Z\"}}"

sysStatus :: Either String (RequestResult SystemStatus)
sysStatus =

  Data.Aeson.eitherDecode responseBody


prettySystemStatus :: SystemStatus -> Doc
prettySystemStatus st =
  colName "instrument"      $$ nest nestCols (text $ show $ status st) $+$
  colName "granularity"     $$ nest nestCols (text $ show $ timestamp st)

