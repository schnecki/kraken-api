{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
module Data.Kraken.TradeState
  ( TradeState(..)
  , prettyTradeState
  ) where


import           Control.DeepSeq
import           Data.Aeson
import           Data.Serialize
import qualified Data.Text        as T
import           GHC.Generics
import           Text.PrettyPrint


data TradeState
  = OPEN                      -- ^ The Trade is currently open
  | CLOSED
  deriving (Show, Read, Eq, Ord, ToJSON, Generic, NFData, Serialize)


instance FromJSON TradeState where
  parseJSON =
    withText "Data.Kraken.TradeState" $ \s ->
      case T.toLower s of
        "open"   -> return OPEN
        "closed" -> return CLOSED
        _        -> fail $ "unexpected string in parseJSON in Data.Kraken.TradeState: " ++ show s


prettyTradeState :: TradeState -> Doc
prettyTradeState OPEN   = text "open"
prettyTradeState CLOSED = text "closed"
