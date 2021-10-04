{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE GADTs          #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds      #-}
{-# LANGUAGE TypeFamilies   #-}
module Data.Kraken.Util
    ( jsonSnakeCase
    , nestCols
    , colName
    , mVal
    , mDefVal
    , tshow
    ) where

import           Data.Aeson
import           Data.Aeson.Casing
import qualified Data.Text         as T
import           Prelude           hiding ((<>))
import           Text.PrettyPrint

jsonSnakeCase :: Options
jsonSnakeCase = defaultOptions { constructorTagModifier = snakeCase }


nestCols :: Int
nestCols = 65


colName :: String -> Doc
colName n = text n <> colon

-- mVal :: Maybe
mVal :: Maybe t -> (t -> Doc) -> Doc
mVal Nothing _     = mempty
mVal (Just v) line = line v

mDefVal :: Doc -> Maybe t -> (t -> Doc) -> Doc
mDefVal def Nothing _   = def
mDefVal _ (Just v) line = line v


tshow :: (Show a) => a -> T.Text
tshow = T.pack . show