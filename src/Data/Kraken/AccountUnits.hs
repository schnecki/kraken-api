{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeSynonymInstances       #-}

module Data.Kraken.AccountUnits
  ( AccountUnits(..)
  , prettyAccountUnits
  ) where

import           Control.DeepSeq
import           Data.Aeson
import           Data.Scientific  (toRealFloat)
import           Data.Serialize
import           Data.Text        (pack, unpack)
import           GHC.Generics
import           Text.PrettyPrint
import           Text.Printf
import           Text.Read        (readMaybe)

newtype AccountUnits =
  AccountUnits Float
  deriving (Generic, Show, Eq, Ord, NFData, Read, Serialize)

instance FromJSON AccountUnits where
  parseJSON (String v) =
    maybe (fail $ "expected number, encounted " ++ unpack v) (return . AccountUnits) (readMaybe $ unpack v)
  parseJSON (Number v) = return $ AccountUnits (toRealFloat v)
  parseJSON v          = fail $ "Cannot parse non string to nubmer (value was '" ++ show v ++ "') in parseJSON of AccountUnits"

instance ToJSON AccountUnits where
  toJSON (AccountUnits v) = String (pack $ printf "%.4f" v)


prettyAccountUnits :: AccountUnits -> Doc
prettyAccountUnits (AccountUnits x) = float x
