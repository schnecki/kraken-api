{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
module Data.Kraken.PositionList
    ( PositionList (..)
    , prettyPositionList
    ) where


import           Control.DeepSeq
import           Data.Aeson
import           Data.Aeson.KeyMap          (toHashMapText)
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
    let oHM = toHashMapText o
    datas <- mapM parseJSON (HM.elems oHM)
    return $ PositionList $ zipWith PositionObject (HM.keys oHM) datas

prettyPositionList :: PositionList -> Doc
prettyPositionList (PositionList xs) = vcat (map prettyPositionObject xs)
