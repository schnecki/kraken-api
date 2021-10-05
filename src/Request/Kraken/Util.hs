

module Request.Kraken.Util where

import           Data.Aeson
import           Data.Char

strToLower :: String -> String
strToLower = fmap toLower
