{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
module Data.Kraken.Position
    ( Position (..)
    , prettyPosition
    , prettyPositionWith
    ) where


import           Control.Applicative        ((<|>))
import           Control.DeepSeq
import           Data.Aeson
-- import           Data.Kraken.PositionDescription
-- import           Data.Kraken.PositionFlags
import           Data.Kraken.OrderStatus
import           Data.Serialize
import           Data.Serialize.Text        ()
import qualified Data.Text                  as T
import           GHC.Generics
import           Text.PrettyPrint

import           Data.Kraken.DateTime
import           Data.Kraken.OrderType
import           Data.Kraken.PriceValue
import           Data.Kraken.TradeOrderType
import           Data.Kraken.Types
import           Data.Kraken.Util

import           Debug.Trace

data Position =
  Position
    { ordertxid      :: T.Text         -- ^ Order ID responsible for the position
    , posstatus      :: PositionStatus -- ^ Value: "open". Position status.
    , pair           :: InstrumentName -- ^ Asset pair
    , time           :: DateTime       -- ^ Unix timestamp of trade
    , orderType      :: OrderType      -- ^ Direction (buy/sell) of position
    , tradeOrderType :: TradeOrderType -- ^ Order type used to open position
    , cost           :: PriceValue     -- ^ Opening cost of position (in quote currency)
    , fee            :: PriceValue     -- ^ Opening fee of position (in quote currency)
    , vol            :: Double         -- ^ Position opening size (in base currency)
    , vol_closed     :: Double         -- ^ Quantity closed (in base currency)
    , margin         :: Double         -- ^ Initial margin consumed (in quote currency)
    , value          :: Maybe Double   -- ^ Current value of remaining position (if docalcs requested)
    , net            :: Maybe Double   -- ^ Unrealised P&L of remaining position (if docalcs requested)
    , terms          :: T.Text         -- ^ Funding cost and term of position
    , rollovertm     :: DateTime       -- ^ Timestamp of next margin rollover fee
    , misc           :: [T.Text]       -- ^ Comma delimited list of add'l info
    , oflags         :: [T.Text]       -- ^ Comma delimited list of opening order flags
    }
  deriving (Read, Show, Eq, Ord, Generic, NFData, ToJSON, Serialize)


instance FromJSON Position where
  parseJSON =
    withObject "Data.Kraken.Position" $ \o -> do
      ordid <- o .: "ordertxid"
      posstatus <- o .: "posstatus"
      pair <- o .: "pair"
      time <- unixTimeStampToDateTime <$> o .: "time"
      ordertype <- o .: "type"
      tOrderType <- o .: "ordertype"
      cost <- o .: "cost"
      fee <- o .: "fee"
      vol <- parseStrToDouble =<< o .: "vol"
      volCl <- parseStrToDouble =<< o .: "vol_closed"
      marg <- parseStrToDouble =<< o .: "margin"
      val <- (fmap Just . parseStrToDouble =<< o .: "value") <|> return Nothing
      net <- (fmap Just . parseStrToDouble =<< o .: "net") <|> return Nothing
      terms <- o .: "terms"
      rollovertm <- unixTimeStampToDateTime <$> (o .: "rollovertm" >>= parseStrToDouble)
      misc <- filter (not . T.null) . T.splitOn "," <$> o .: "misc"
      oflags <- filter (not . T.null) . T.splitOn "," <$> o .: "oflags"
      return $ Position ordid posstatus pair time ordertype tOrderType cost fee vol volCl marg val net terms rollovertm misc oflags


prettyPosition :: Position -> Doc
prettyPosition = prettyPositionWith 0

prettyPositionWith :: Int -> Position -> Doc
prettyPositionWith nesting pos =
    colName "ordertxid"                     $$ nest n2 (prettyText  $ ordertxid pos) $+$
    colName "posstatus"                     $$ nest n2 (prettyOrderStatus  $ posstatus pos) $+$
    colName "pair"                          $$ nest n2 (prettyText  $ pair pos) $+$
    colName "time"                          $$ nest n2 (prettyDateTime  $ time pos) $+$
    colName "orderType"                     $$ nest n2 (prettyOrderType  $ orderType pos) $+$
    colName "tradeOrderType"                $$ nest n2 (prettyTradeOrderType  $ tradeOrderType pos) $+$
    colName "cost"                          $$ nest n2 (prettyPriceValue  $ cost pos) $+$
    colName "fee"                           $$ nest n2 (prettyPriceValue  $ fee pos) $+$
    colName "vol"                           $$ nest n2 (prettyDouble  $ vol pos) $+$
    colName "vol_closed"                    $$ nest n2 (prettyDouble  $ vol_closed pos) $+$
    colName "margin"                        $$ nest n2 (prettyDouble  $ margin pos) $+$
    mVal (value pos) (\v -> colName "value" $$ nest n2 (prettyDouble v)) $+$
    mVal (net pos) (\v -> colName "net"     $$ nest n2 (prettyDouble v)) $+$
    colName "terms"                         $$ nest n2 (prettyText  $ terms pos) $+$
    colName "rollovertm"                    $$ nest n2 (prettyDateTime  $ rollovertm pos) $+$
    colName "misc"                          $$ nest n2 (hsep $ punctuate (text ", ") $ map prettyText $ misc pos) $+$
    colName "oflags"                        $$ nest n2 (hsep $ punctuate (text ", ") $ map prettyText $ oflags pos)
  where n2 = nestCols - nesting
