
-- | Fake module until we adapt the version >=2.0 of Aeson
module Data.Aeson.Key
    ( fromString
    ) where

import qualified Data.Text as T

fromString :: String -> T.Text
fromString = T.pack
