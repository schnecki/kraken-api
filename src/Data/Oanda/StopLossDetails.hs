{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}


module Data.Oanda.StopLossDetails
    ( StopLossDetails (..)
    ) where

import           Control.DeepSeq
import           Data.Aeson
import           GHC.Generics

import           Data.Oanda.ClientExtensions
import           Data.Oanda.DateTime
import           Data.Oanda.DecimalNumber
import           Data.Oanda.PriceValue
import           Data.Oanda.TimeInForce

data StopLossDetails = StopLossDetails
  { price            :: PriceValue       -- ^ The price that the Stop Loss Order will be triggered at. Only one of the
                                         -- price and distance fields may be specified.
  , distance         :: DecimalNumber    -- ^ Specifies the distance (in price units) from the Trade’s open price to use
                                         -- as the Stop Loss Order price. Only one of the distance and price fields may
                                         -- be specified.
  , timeInForce      :: TimeInForce      -- ^ The time in force for the created Stop Loss Order. This may only be GTC,
                                         -- GTD or GFD. [default=GTC]
  , gtdTime          :: DateTime         -- ^ The date when the Stop Loss Order will be cancelled on if timeInForce is
                                         -- GTD.
  , clientExtensions :: ClientExtensions -- ^ The Client Extensions to add to the Stop Loss Order when created.
  , guaranteed       :: Bool             -- ^ Flag indicating that the price for the Stop Loss Order is guaranteed. The
                                         -- default value depends on the GuaranteedStopLossOrderMode of the account, if
                                         -- it is REQUIRED, the default will be true, for DISABLED or ENABLED the
                                         -- default is false.
  } deriving (Show, Eq, Ord, FromJSON, ToJSON, Generic, NFData)
