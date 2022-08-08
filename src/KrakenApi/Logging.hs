{-# LANGUAGE TemplateHaskell #-}
module KrakenApi.Logging
    ( enableKrakenApiLogging
    , disableKrakenApiLogging
    ) where

import           ApiMaker   (disableRequestLogging, enableRequestLogging)
import           EasyLogger

enableKrakenApiLogging :: LogDestination -> IO ()
enableKrakenApiLogging = do
  $(initLogger)
  enableRequestLogging

disableKrakenApiLogging :: IO ()
disableKrakenApiLogging = do
  $(finalizeLogger)
  disableRequestLogging
