{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
module Data.Kraken.RequestResult
    ( RequestResult (..)
    , RequestError (..)
    , prettyRequestResult
    , fromRequestResult
    ) where


import           ApiMaker
import           Control.DeepSeq
import           Control.Monad.Except
import           Data.Aeson
import           Data.Serialize
import           Data.Serialize.Text      ()
import qualified Data.Text                as T
import           GHC.Generics
import           Text.PrettyPrint         hiding ((<>))

import           Data.Kraken.RequestError
import           Data.Kraken.Util


data RequestResult a = RequestResult
  { error  :: [RequestError]         -- ^ The system status
  , result :: a
  } deriving (Show, Read, Eq, Ord, Serialize, Generic, FromJSON, NFData)


fromRequestResult :: (MonadError SafeException m) => RequestResult a -> m a
fromRequestResult (RequestResult [] res) = return res
fromRequestResult (RequestResult err _)  = throwUserException $ EUnknownError $ T.intercalate ";" $ map tshow err


prettyRequestResult :: (a -> Doc) -> RequestResult a -> Doc
prettyRequestResult pRes res =
  colName "error"      $$ nest nestCols (text $ show $ Data.Kraken.RequestResult.error res) $+$
  colName "result"     $$ nest nestCols (pRes $ result res)

