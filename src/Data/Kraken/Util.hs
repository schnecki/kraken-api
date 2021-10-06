{-# LANGUAGE DataKinds    #-}
{-# LANGUAGE GADTs        #-}
{-# LANGUAGE PolyKinds    #-}
{-# LANGUAGE TypeFamilies #-}
module Data.Kraken.Util
    ( jsonSnakeCase
    , nestCols
    , colName
    , mVal
    , mDefVal
    , tshow
    , hsepWith
    , hsepComma
    ) where

import           Data.Aeson
import           Data.Aeson.Casing
import           Data.List         (intersperse)
import qualified Data.Text         as T
import           Prelude           hiding ((<>))
import           Text.PrettyPrint

jsonSnakeCase :: Options
jsonSnakeCase = defaultOptions { constructorTagModifier = snakeCase, fieldLabelModifier = snakeCase }

-- jsonCamelCase :: Options
-- jsonCamelCase = defaultOptions { constructorTagModifier = snakeCase }

hsepWith :: Doc -> [Doc] -> Doc
hsepWith x xs = hsep (intersperse x xs)

hsepComma :: [Doc] -> Doc
hsepComma = hsepWith (char ',')


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
