{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
module Data.Kraken.SpreadList
    ( SpreadList(..)
    , prettySpreadList
    ) where

import           Control.DeepSeq
import           Data.Aeson
import           Data.Aeson.Key
import qualified Data.Aeson.KeyMap        as HM
import           Data.Serialize
import qualified Data.Vector              as V
import           GHC.Generics
import           Text.PrettyPrint

import           Data.Kraken.SpreadObject
import           Data.Kraken.Util

data SpreadList =
  SpreadList
    { spreadLast :: Integer
    , spreadList :: [SpreadObject]
    }
  deriving (Read, Show, Eq, Ord, Generic, NFData, Serialize)


instance FromJSON SpreadList where
  parseJSON =
    withObject "Data.Kraken.SpreadList" $ \o -> do
      last' <- parseJSON =<< parseStrToNum =<< (o .: "last")
      datas <- mapM (withArray "Data.Kraken.SpreadList parsing Spreads" $ fmap V.toList . mapM parseJSON) (mkElems o)
      return $ SpreadList last' $ zipWith SpreadObject (mkKeys o) datas
    where
      mkKeys o = filter (/= "last") (map toText . HM.keys $ o)
      mkElems o = HM.elems $ HM.filterWithKey (\k _ -> k /= "last") o


prettySpreadList :: SpreadList -> Doc
prettySpreadList (SpreadList last' xs) =
  colName "Spread Last ID" $$ nest nestCols (integer last') $+$
  vcat (map prettySpreadObject xs)
