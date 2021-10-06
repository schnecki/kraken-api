{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeSynonymInstances       #-}
module Data.Kraken.OrderMinimum
    ( OrderMinimum (..)
    , prettyOrderMinimum
    ) where

import           Control.DeepSeq
import           Data.Aeson
import           Data.Scientific
import           Data.Serialize
import           Data.Text        (pack, unpack)
import           GHC.Generics
import           Text.PrettyPrint
import           Text.Printf
import           Text.Read        (readMaybe)


newtype OrderMinimum =
  OrderMinimum
    { fromOrderMinimum :: Float
    }
  deriving (Show, Read, Eq, Ord, Serialize, Generic, NFData, Num, Fractional)

instance FromJSON OrderMinimum where
  parseJSON (String v) = maybe (fail $ "expected number, encounted " ++ unpack v) (return . OrderMinimum) (readMaybe $ unpack v)
  parseJSON (Number v) = return $ OrderMinimum (toRealFloat v)
  parseJSON v = fail $ "Cannot parse non string to nubmer (value was '" ++ show v ++ "') in parseJSON of OrderMinimum"

instance ToJSON OrderMinimum where
  toJSON (OrderMinimum x) = String $ pack (printf "%.4f" x :: String)

prettyOrderMinimum :: OrderMinimum -> Doc
prettyOrderMinimum = text . showOrderMinimum

showOrderMinimum :: OrderMinimum -> String
showOrderMinimum (OrderMinimum f) = printf "%.4f" f


