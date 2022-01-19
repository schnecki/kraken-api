{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
module Data.Kraken.PositionList
    ( PositionList (..)
    , prettyPositionList
    ) where


import           Control.DeepSeq
import           Data.Aeson
import qualified Data.HashMap.Strict        as HM
import           Data.Serialize
import           GHC.Generics
import           Text.PrettyPrint

import           Data.Kraken.PositionObject


data PositionList =
  PositionList
    { positions :: [PositionObject]
    }
  deriving (Read, Show, Eq, Ord, Generic, NFData, Serialize)

instance FromJSON PositionList where
  parseJSON =
    withObject "Data.Kraken.PositionList" $ \o -> do
    datas <- mapM parseJSON (HM.elems o)
    return $ PositionList $ zipWith PositionObject (HM.keys o) datas

prettyPositionList :: PositionList -> Doc
prettyPositionList (PositionList xs) = vcat (map prettyPositionObject xs)
