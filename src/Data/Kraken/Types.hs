module Data.Kraken.Types
    ( InstrumentName
    , PairName
    ) where

import           Data.Text (Text)

type InstrumentName = Text      -- ^ A string containing the base currency.
type PairName = InstrumentName  -- ^ A string containing the pair.
