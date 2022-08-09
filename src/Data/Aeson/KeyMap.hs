
-- | Fake module until we adapt the version >=2.0 of Aeson
module Data.Aeson.KeyMap
    ( toHashMapText
    , insert
    , alterF
    ) where

-- Todo, when switching to GHC >=9.0, use actual aeson import:
-- import           Data.Aeson.KeyMap           (toHashMapText)

import           Data.Aeson
import qualified Data.HashMap.Strict as HM
import qualified Data.Text           as T

toHashMapText :: HM.HashMap T.Text Value -> HM.HashMap T.Text Value
toHashMapText = id


insert :: T.Text -> Value -> HM.HashMap T.Text Value -> HM.HashMap T.Text Value
insert = HM.insert

alterF :: (Functor f) => (Maybe Value -> f (Maybe Value)) -> T.Text -> HM.HashMap T.Text Value -> f (HM.HashMap T.Text Value)
alterF = HM.alterF
