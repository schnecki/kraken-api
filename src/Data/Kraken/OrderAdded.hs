{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Data.Kraken.OrderAdded
    ( OrderAdded (..)
    , prettyOrderAdded
    ) where

import           Control.Applicative ((<|>))
import           Control.DeepSeq
import           Data.Aeson
import           Data.Serialize
import           Data.Serialize.Text ()
import qualified Data.Text           as T
import           EasyLogger
import           GHC.Generics
import           Text.PrettyPrint

import           Data.Kraken.Util

data OrderAdded  = OrderAdded
  { order :: T.Text
  , close :: Maybe T.Text
  , txid  :: Maybe T.Text -- ^ Only if order was added successfully
  }
  deriving (Read, Show, Eq, Ord, Generic, NFData, Serialize)

instance ToJSON OrderAdded where
  toJSON = genericToJSON lowerCase

instance FromJSON OrderAdded where
  parseJSON =
    withObject "Data.Kraken.OrderAdded" $ \o ->
      $(pureLogDebug) ("Data.Kraken.OrderAdded parseJSON input: " ++ show o) $ do
        (order, close) <- o .: "descr" >>= parseOrderClose
        txid <- o .: "txid" <|> return Nothing
        return $ OrderAdded order close txid
    where
      parseOrderClose o' = do
        order <- o' .: "order"
        close <- (o' .: "close" >>= parseToMaybeText) <|> return Nothing
        return (order, close)


prettyOrderAdded :: OrderAdded -> Doc
prettyOrderAdded o =
  colName "order desc"                             $$ nest nestCols (prettyText $ order o) $+$
  mVal (close o) (\v -> colName "close order desc" $$ nest nestCols (prettyText v)) $+$
  mVal (txid o) (\v -> colName "Transaction ID"    $$ nest nestCols (prettyText v))
