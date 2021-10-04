{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
module Data.Kraken.RequestError
    ( RequestError (..)

    ) where


import           Control.DeepSeq
import           Data.Aeson
import           Data.Serialize
import           Data.Serialize.Text ()
import qualified Data.Text           as T
import           GHC.Exception.Type
import           GHC.Generics


-- Error Details

-- HTTP status codes are generally not used by our API to convey information about the state of requests -- any errors or warnings are denoted in the error field of the response as described above. Status codes other than 200 indicate that there was an issue with the request reaching our servers.

-- error messages follow the general format <severity><category>:<error msg>[:add'l text]

--     severity can be either E for error or W for warning
--     category can be one of General, Auth, API, Query, Order, Trade, Funding, or Service
--     error msg can be any text string that describes the reason for the error


data RequestError =
   EGeneralInvalidArguments         -- ^ The request payload is malformed, incorrect or ambiguous.
 | EServiceUnavailable              -- ^ The matching engine or API is offline
 | EServiceMarketInCancelOnlymode   -- ^ Request can't be made at this time. (See SystemStatus endpoint.)
 | EServiceMarketInPostOnlyMode     -- ^ Request can't be made at this time. (See SystemStatus endpoint.)
 | EAPIInvalidKey                   -- ^ An invalid API-Key header was supplied (see Authentication section)
 | EAPIInvalidSignature             -- ^ An invalid API-Sign header was supplied (see Authentication section)
 | EAPIInvalidNonce                 -- ^ An invalid nonce was supplied (see Authentication section)
 | EGeneralPermissionDenied         -- ^ API key doesn't have permission to make this request.
 | EOrderCannotOpenPosition         -- ^ User/tier is ineligible for margin trading
 | EOrderMarginAllowanceExceeded    -- ^ User has exceeded their margin allowance
 | EOrderMarginLevelTooLow          -- ^ Client has insufficient equity or collateral
 | EOrderMarginPositionSizeExceeded -- ^ Client would exceed the maximum position size for this pair
 | EOrderInsufficientMargin         -- ^ Exchange does not have available funds for this margin trade
 | EOrderInsufficientFunds          -- ^ Client does not have the necessary funds
 | EOrderOrderMinimumNotMet         -- ^ Order size does not meet ordermin. (See AssetPairs endpoint.)
 | EOrderOrdersLimitExceeded        -- ^ (See Rate Limits section)
 | EOrderRateLimitExceeded          -- ^ (See Rate Limits section)
 | EOrderPositionsLimitExceeded
 | EOrderUnknownPosition
 | EUnknownError T.Text
  deriving (Show, Read, Eq, Ord, Serialize, Generic, NFData)


instance Exception RequestError

instance FromJSON RequestError where
  parseJSON (String v) = return $ EUnknownError ("TODO: Implemented parseJSON from RequestError!!!\n" <> v)
  parseJSON v = fail $ "Cannot parse non string to UTCTime (value was '" ++ show v ++ "') in parseJSON of DateTime"


