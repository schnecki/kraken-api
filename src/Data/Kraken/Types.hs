module Data.Kraken.Types
    ( InstrumentName
 
    ) where

import Data.Text (Text) 

type InstrumentName = Text      -- ^ A string containing the base currency and quote currency delimited by a “_”.
