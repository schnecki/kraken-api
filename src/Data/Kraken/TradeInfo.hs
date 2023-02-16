{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Data.Kraken.TradeInfo
    ( TradeInfo (..)
    , prettyTradeInfo
    , prettyTradeInfoWith
    ) where


import           Control.Applicative        ((<|>))
import           Control.DeepSeq
import           Control.Monad.Trans
import           Data.Aeson
import           Data.Aeson.Key
import           Data.Serialize
import           Data.Serialize.Text        ()
import qualified Data.Text                  as T
import           EasyLogger
import           GHC.Generics
import           Text.PrettyPrint

import           Data.Kraken.DateTime
import           Data.Kraken.OrderType
import           Data.Kraken.PriceValue
import           Data.Kraken.TradeOrderType
import           Data.Kraken.TradeState
import           Data.Kraken.Types
import           Data.Kraken.Util


data TradeInfo =
  TradeInfo
    {  ordertxid :: T.Text           -- ^ Order responsible for execution of trade
    ,  pair      :: InstrumentName   -- ^ Asset pair
    ,  time      :: DateTime         -- ^ Unix timestamp of trade
    ,  tradeType :: OrderType        -- ^ Type of order (buy/sell)
    ,  ordertype :: TradeOrderType   -- ^ Order type
    ,  price     :: PriceValue       -- ^ Average price order was executed at (quote currency)
    ,  cost      :: PriceValue       -- ^ Total cost of order (quote currency)
    ,  fee       :: PriceValue       -- ^ Total fee (quote currency)
    ,  vol       :: Double           -- ^ Volume (base currency)
    ,  margin    :: Double           -- ^ Initial margin (quote currency)
    ,  misc      :: [T.Text]         -- ^ Comma delimited list of miscellaneous info: closing — Trade closes all or part of a position
    ,  posstatus :: Maybe TradeState -- ^ Position status (open/closed). Only present if trade opened a position
    ,  cprice    :: Maybe PriceValue -- ^ Average price of closed portion of position (quote currency). Only present if trade opened a position
    ,  ccost     :: Maybe PriceValue -- ^ Total cost of closed portion of position (quote currency). Only present if trade opened a position
    ,  cfee      :: Maybe PriceValue -- ^ Total fee of closed portion of position (quote currency). Only present if trade opened a position
    ,  cvol      :: Maybe Double     -- ^ Total fee (sic! volume?!) of closed portion of position (quote currency). Only present if trade opened a position
    ,  cmargin   :: Maybe Double     -- ^ Total margin freed in closed portion of position (quote currency). Only present if trade opened a position
    ,  net       :: Maybe T.Text     -- ^ Net profit/loss of closed portion of position (quote currency, quote currency scale). Only present if trade opened a position
    ,  trades    :: Maybe [T.Text]   -- ^ List of closing trades for position (if available). Only present if trade opened a position
    }
  deriving (Read, Show, Eq, Ord, Generic, NFData, ToJSON, Serialize)


instance FromJSON TradeInfo where
  parseJSON =
    withObject "Data.Kraken.TradeInfo" $ \o ->
    $(pureLogDebug) ("Data.Kraken.TradeInfo parseJSON input: " ++ show o) $ do
      tx <- o .: "ordertxid"
      pa <- o .: "pair"
      ti <- unixTimeStampToDateTime <$> o .: "time"
      tp <- o .: "type"
      ot <- o .: "ordertype"
      pr <- o .: "price"
      co <- o .: "cost"
      fe <- o .: "fee"
      vo <- o .: "vol" >>= parseStrToDouble
      ma <- o .: "margin"  >>= parseStrToDouble
      mis <- T.splitOn "," <$> o .: "misc"
      poss <- (o .: "posstatus" >>= parseJSON) <|> return Nothing
      cpr <- o .: "cprice" <|> return Nothing
      ccost <- o .: "ccost" <|> return Nothing
      cfee <- o .: "cfee" <|> return Nothing
      cvol <- (o .: "cvol" >>= parseMaybeStrToDouble)  <|> return Nothing
      cmargin <- (o .: "cmargin" >>= parseMaybeStrToDouble)  <|> return Nothing
      net <- o .: "cnet"  <|> return Nothing
      trades <- o .: "trades" <|> return Nothing
      return $ TradeInfo tx pa ti tp ot pr co fe vo ma mis poss cpr ccost cfee cvol cmargin net trades
    -- where
    --   parseList = mapM parseJSON . (map String . filter (not . T.null) . T.splitOn ",")


prettyTradeInfo :: TradeInfo -> Doc
prettyTradeInfo = prettyTradeInfoWith 0

prettyTradeInfoWith :: Int -> TradeInfo -> Doc
prettyTradeInfoWith nesting ord =
  colName "ordertxid"                             $$ nest n2 (prettyText $ ordertxid ord) $+$
  colName "pair"                                  $$ nest n2 (prettyText $ pair ord) $+$
  colName "time"                                  $$ nest n2 (prettyDateTime $ time ord) $+$
  colName "tradeType"                             $$ nest n2 (prettyOrderType $ tradeType ord) $+$
  colName "ordertype"                             $$ nest n2 (prettyTradeOrderType $ ordertype ord) $+$
  colName "price"                                 $$ nest n2 (prettyPriceValue $ price ord) $+$
  colName "cost"                                  $$ nest n2 (prettyPriceValue $ cost ord) $+$
  colName "fee"                                   $$ nest n2 (prettyPriceValue $ fee ord) $+$
  colName "vol"                                   $$ nest n2 (prettyDouble $ vol ord) $+$
  colName "margin"                                $$ nest n2 (prettyDouble $ margin ord) $+$
  colName "misc"                                  $$ nest n2 (hsep $ punctuate (text ", ") $ map prettyText $ misc ord) $+$
  mVal (posstatus ord) (\v -> colName "posstatus" $$ nest n2 (prettyTradeState v)) $+$
  mVal (cprice ord) (\v -> colName "cprice"       $$ nest n2 (prettyPriceValue v)) $+$
  mVal (ccost ord) (\v -> colName "ccost"         $$ nest n2 (prettyPriceValue v)) $+$
  mVal (cfee ord) (\v -> colName "cfee"           $$ nest n2 (prettyPriceValue v)) $+$
  mVal (cvol ord) (\v -> colName "cvol"           $$ nest n2 (prettyDouble v)) $+$
  mVal (cmargin ord) (\v -> colName "cmargin"     $$ nest n2 (prettyDouble v)) $+$
  mVal (net ord) (\v -> colName "net"             $$ nest n2 (prettyText v)) $+$
  mVal (trades ord) (\v -> colName "trades"       $$ nest n2 (hsep $ punctuate (text ", ") $ map prettyText v))

  where n2 = nestCols - nesting
