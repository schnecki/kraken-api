{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
module Data.Kraken.DateTime
    ( DateTime (..)
    , fromDateTime
    , dateTimeToPOSIXTime
    , posixTimeToDateTime
    , secondsToDateTime
    , prettyDateTime
    ) where

import           Control.DeepSeq
import           Data.Aeson
import           Data.Serialize
import           Data.Time
import           Data.Time.Clock.POSIX
import           Data.Time.Clock.Serialize ()
import           Data.Time.RFC3339
import           GHC.Generics
import           Text.PrettyPrint

newtype DateTime =
  DateTime (Maybe UTCTime)
  deriving (Generic, Serialize, Eq, Ord, NFData)

fromDateTime :: DateTime -> Maybe UTCTime
fromDateTime (DateTime mUtc) = mUtc

instance Show DateTime where
  show (DateTime (Just time)) = formatTimeRFC3339 (utcToZonedTime utc time)
  show (DateTime Nothing)     = "0"

prettyDateTime :: DateTime -> Doc
prettyDateTime = text . show

instance Read DateTime where
  readsPrec _ ('0':xs) = [(DateTime Nothing, xs)]
  readsPrec _ str = [(DateTime $ zonedTimeToUTC <$> parseTimeRFC3339 bef, aft)]
    where
      (bef, aft) = span (/= ' ') str

instance FromJSON DateTime where
  parseJSON (String v) = return $ DateTime $ zonedTimeToUTC <$> parseTimeRFC3339 v
  parseJSON v = fail $ "Cannot parse non string to UTCTime (value was '" ++ show v ++ "') in parseJSON of DateTime"

instance ToJSON DateTime where
  toJSON (DateTime Nothing)     = String "0"
  toJSON (DateTime (Just time)) = String $ formatTimeRFC3339 (utcToZonedTime utc time)


dateTimeToPOSIXTime :: DateTime -> POSIXTime
dateTimeToPOSIXTime (DateTime (Just time)) = utcTimeToPOSIXSeconds time
dateTimeToPOSIXTime (DateTime Nothing)     = 0

posixTimeToDateTime :: POSIXTime -> DateTime
posixTimeToDateTime = DateTime . Just . posixSecondsToUTCTime


secondsToDateTime :: Integer -> DateTime
secondsToDateTime = posixTimeToDateTime . fromIntegral
