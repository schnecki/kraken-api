{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Data.Kraken.ClosedOrderList
    ( ClosedOrderList (..)
    , prettyClosedOrderList
    ) where


import           Control.DeepSeq
import           Data.Aeson
import           Data.Aeson.Key
import qualified Data.Aeson.KeyMap                  as HM
import           Data.Serialize
import qualified Data.Vector                        as V
import           EasyLogger
import           GHC.Generics
import           Text.PrettyPrint

import           Data.Kraken.CandlestickGranularity
import           Data.Kraken.ClosedOrderObject
import           Data.Kraken.Util

import           Debug.Trace

data ClosedOrderList =
  ClosedOrderList
    { closedOrders :: [ClosedOrderObject]
    }
  deriving (Read, Show, Eq, Ord, Generic, NFData, Serialize)

instance FromJSON ClosedOrderList where
  parseJSON =
    withObject "Data.Kraken.ClosedOrderList" $ \closed ->
      $(pureLogDebug) ("Data.Kraken.ClosedOrderList parseJSON input: " ++ show closed) $ do
        let mClosed = HM.lookup "closed" closed
        flip (maybe (return $ ClosedOrderList [])) mClosed $
          withObject "Data.Kraken.ClosedOrderList closed" $ \o -> do
            datas <- mapM parseJSON (HM.elems o)
            return $ ClosedOrderList $ zipWith ClosedOrderObject (map toText . HM.keys $ o) datas

prettyClosedOrderList :: ClosedOrderList -> Doc
prettyClosedOrderList (ClosedOrderList xs) = vcat (map prettyClosedOrderObject xs)
