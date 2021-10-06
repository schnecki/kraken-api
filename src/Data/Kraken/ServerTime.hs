{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
module Data.Kraken.ServerTime
    ( ServerTime (..)
    , prettyServerTime
    ) where


import           Control.DeepSeq
import           Data.Aeson
import           Data.Serialize
import           Data.Serialize.Text ()
import qualified Data.Text           as T
import           GHC.Generics
import           Text.PrettyPrint


import           Data.Kraken.Util


data ServerTime = ServerTime
  { unixtime :: Integer
  , rfc1123  :: T.Text
  }
  deriving (Read, Show, ToJSON, FromJSON, Eq, Ord, Generic, NFData, Serialize)

prettyServerTime :: ServerTime -> Doc
prettyServerTime (ServerTime unix rfc) =
  colName "unixtime" $$ nest nestCols (integer unix) $+$
  colName "rfc1123"  $$ nest nestCols (text $ T.unpack rfc)

