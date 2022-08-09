{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
module Data.Kraken.OpenOrderList
    ( OpenOrderList (..)
    , prettyOpenOrderList
    ) where


import           Control.DeepSeq
import           Data.Aeson
import           Data.Aeson.KeyMap           (toHashMapText)
import qualified Data.HashMap.Strict         as HM (elems, keys, lookup)
import           Data.Serialize
import           GHC.Generics
import           Text.PrettyPrint

import           Data.Kraken.OpenOrderObject


data OpenOrderList =
  OpenOrderList
    { openOrders :: [OpenOrderObject]
    }
  deriving (Read, Show, Eq, Ord, Generic, NFData, Serialize)

instance FromJSON OpenOrderList where
  parseJSON =
    withObject "Data.Kraken.OpenOrderList" $ \open -> do
      let mOpen = HM.lookup "open" (toHashMapText open)
      flip (maybe (return $ OpenOrderList [])) mOpen $
        withObject "Data.Kraken.OpenOrderList open" $ \o -> do
          let oHM = toHashMapText open
          datas <- mapM parseJSON (HM.elems oHM)
          return $ OpenOrderList $ zipWith OpenOrderObject (HM.keys $ oHM) datas

prettyOpenOrderList :: OpenOrderList -> Doc
prettyOpenOrderList (OpenOrderList xs) = vcat (map prettyOpenOrderObject xs)
