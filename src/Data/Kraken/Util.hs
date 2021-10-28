{-# LANGUAGE DataKinds    #-}
{-# LANGUAGE GADTs        #-}
{-# LANGUAGE PolyKinds    #-}
{-# LANGUAGE TypeFamilies #-}
module Data.Kraken.Util
    ( parseStrToNum
    , parseStrToDouble
    , jsonSnakeCase
    , nestCols
    , nestIndent
    , colName
    , mVal
    , mDefVal
    , tshow
    , hsepWith
    , hsepComma
    , prettyText
    , prettyDouble
    ) where

import           Data.Aeson
import           Data.Aeson.Casing
import           Data.Aeson.Types
import           Data.List         (intersperse)
import qualified Data.Text         as T
import           Prelude           hiding ((<>))
import           Text.PrettyPrint
import           Text.Read         (readMaybe)


parseStrToNum :: Value -> Parser Value
parseStrToNum (String v) = maybe (fail $ "Could not convert string to Number. String: " ++ show v) (return . Number) (readMaybe $ T.unpack v)
parseStrToNum x@Number{} = return x
parseStrToNum v          = fail $ "Expected string of number or number, but encountered : " ++ show v


parseStrToDouble :: Value -> Parser Double
parseStrToDouble v = parseJSON =<< parseStrToNum v


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

nestIndent :: Int
nestIndent = 10


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

prettyText :: T.Text -> Doc
prettyText = text . T.unpack

prettyDouble :: Double -> Doc
prettyDouble = double
