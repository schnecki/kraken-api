{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
module Data.Kraken.AccountBalanceList
    ( AccountBalanceList (..)
    , prettyAccountBalanceList
    ) where


import           Control.DeepSeq
import           Data.Aeson
import           Data.Aeson.Key
import qualified Data.Aeson.KeyMap          as HM
import           Data.HashMap.Strict        (elems, keys)
import           Data.Serialize
import           GHC.Generics
import           Text.PrettyPrint

import           Data.Kraken.AccountBalance


data AccountBalanceList =
  AccountBalanceList
    { accountBalances :: [AccountBalance]
    }
  deriving (Read, Show, Eq, Ord, Generic, NFData, Serialize)


instance FromJSON AccountBalanceList where
  parseJSON val = withObject "Data.Kraken.AccountBalanceList" (\o -> AccountBalanceList . zipWith AccountBalance (map toText . HM.keys $ o) <$> mapM parseJSON (HM.elems o)) val


prettyAccountBalanceList :: AccountBalanceList -> Doc
prettyAccountBalanceList (AccountBalanceList xs) = vcat (map prettyAccountBalance xs)
