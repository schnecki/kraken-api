{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
module Data.Kraken.OrderBookList
    ( OrderBookList (..)
    , prettyOrderBookList
    ) where


import           Control.DeepSeq
import           Control.Monad               (zipWithM)
import           Data.Aeson
import           Data.Aeson.Key
import qualified Data.Aeson.KeyMap           as HM
import           Data.Serialize
import           GHC.Generics
import           Text.PrettyPrint

import           Data.Kraken.OrderBookObject


data OrderBookList =
  OrderBookList
    { orderBookData :: [OrderBookObject]
    }
  deriving (Read, Show, Eq, Ord, Generic, NFData, Serialize)


instance FromJSON OrderBookList where
  parseJSON = withObject "Data.Kraken.OrderBookList" $ \o -> OrderBookList <$> zipWithM f (map toText . HM.keys $ o) (HM.elems o)
    where
      f k =
        withObject "Data.Kraken.OrderBookList parsing OrderBookObject" $ \x -> do
          as <- x .: "asks"
          bs <- x .: "bids"
          return $ OrderBookObject k as bs


prettyOrderBookList :: OrderBookList -> Doc
prettyOrderBookList (OrderBookList xs) = vcat (map prettyOrderBookObject xs)
