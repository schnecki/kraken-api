{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
module Data.Kraken.OpenOrderList
    ( OpenOrderList (..)
    , prettyOpenOrderList
    ) where


import           Control.DeepSeq
import           Data.Aeson
import           Data.Aeson.Key
import qualified Data.Aeson.KeyMap           as HM
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
      let mOpen = HM.lookup "open" open
      flip (maybe (return $ OpenOrderList [])) mOpen $
        withObject "Data.Kraken.OpenOrderList open" $ \o -> do
          datas <- mapM parseJSON (HM.elems o)
          return $ OpenOrderList $ zipWith OpenOrderObject (map toText . HM.keys $ o) datas

prettyOpenOrderList :: OpenOrderList -> Doc
prettyOpenOrderList (OpenOrderList xs) = vcat (map prettyOpenOrderObject xs)
