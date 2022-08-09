{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
module Data.Kraken.SpreadList
    ( SpreadList(..)
    , prettySpreadList
    ) where

import           Control.DeepSeq
import           Data.Aeson
import           Data.Aeson.KeyMap        (toHashMapText)
import qualified Data.HashMap.Strict      as HM (elems, filterWithKey, keys)
import           Data.Serialize
import qualified Data.Vector              as V
import           GHC.Generics
import           Text.PrettyPrint

import           Data.Kraken.SpreadObject
import           Data.Kraken.Util

data SpreadList =
  SpreadList
    { speadLast :: Integer
    , speadList :: [SpreadObject]
    }
  deriving (Read, Show, Eq, Ord, Generic, NFData, Serialize)


instance FromJSON SpreadList where
  parseJSON =
    withObject "Data.Kraken.SpreadList" $ \o -> do
      last' <- parseJSON =<< parseStrToNum =<< (o .: "last")
      let oHM = toHashMapText o
      datas <- mapM (withArray "Data.Kraken.SpreadList parsing Spreads" $ \o' -> V.toList <$> mapM parseJSON o') (mkElems oHM)
      return $ SpreadList last' $ zipWith SpreadObject (mkKeys oHM) datas
    where
      mkKeys o = filter (/= "last") (HM.keys o)
      mkElems o = HM.elems $ HM.filterWithKey (\k _ -> k /= "last") o


prettySpreadList :: SpreadList -> Doc
prettySpreadList (SpreadList last' xs) =
  colName "Spread Last ID" $$ nest nestCols (integer last') $+$
  vcat (map prettySpreadObject xs)
