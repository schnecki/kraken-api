{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Main where

import           ApiMaker
import           Control.Monad             (forM, forM_, void)
import           Control.Monad.Except
import           Control.Monad.Trans       (liftIO)
import           Control.Monad.Trans.State
import           Data.Aeson                (encode)
import qualified Data.ByteString           as B
import qualified Data.ByteString.Char8     as C
import qualified Data.Text                 as T
import           Data.Time.Clock
import qualified Data.Word                 as W8
import           Logging
import           Prelude                   hiding (id)

-- import           Data.Kraken.AccountProperties
-- import           Data.Kraken.Accounts
import           Data.Kraken.DateTime
-- import           Data.Kraken.Instrument
-- import           Data.Kraken.OrderRequest
import           KrakenApi

main :: IO ()
main = do
  $(initLogger) (LogFile "borl-trader.log") LogDebug
  $(logInfo) ("Starting App" :: T.Text)

  apiKey <- C.filter (/= '\n') <$> B.readFile "API_KEY"
  putStrLn $ "API_KEY: " <> show apiKey
  let cfg = krakenConfigTradeAccount apiKey
  res <-
    runSessReqWithParamsM (additionalParams cfg) cfg $ runRequests $ do
      res <- mkReq GetSystemStatus
      liftIO $ putStrLn $ take 100 $ show res
      res <- mkReq $ GetAssetInfo "INJ,ADA" Nothing
      liftIO $ print res

      -- accs <- mkReq GetAccounts
      -- liftIO $ print accs
      -- forM_ (accounts accs) $ \prop -> do
      --   let accId = id prop
      --   res <- mkReq $ GetSystemStatus
      --   liftIO $ putStrLn $ take 100 $ show res
        -- res <- mkReq $ GetAccountSummary accId
        -- liftIO $ putStrLn $ take 100 $ show res
        -- insts <- mkReq $ GetInstruments accId Nothing
        -- liftIO $ putStrLn $ take 100 $ show insts
        -- let config = AccountConfigurationUpdate (Just "Test Account Nr uno") (Just 1)
        -- liftIO $ print $ "Trying to set following values: " <> encode config
        -- res <- mkReq $ PatchAccountConfiguration accId config
        -- liftIO $ putStrLn $ take 100 $ show res
        -- now <- liftIO getCurrentTime
        -- let other = addUTCTime (negate $ fromIntegral $ 60 * 60 * 24 * 365 * 8) now
        -- let candleCfg = CandleConfig (Just "M") (Just S5) (Just 6) (Just $ DateTime $ Just other)
        --       Nothing Nothing Nothing Nothing Nothing Nothing -- count=6&price=M&granularity=S5
        -- res <- mkReq $ GetInstrumentCandle "EUR_USD" candleCfg
        -- liftIO $ print res
      --   liftIO $ putStrLn $ take 100 $ show res
      --   let orderReq = marketOrder "EUR_USD" 1.0 FOKMarketOrder 0.8
      --   liftIO $ print $ encode orderReq
      -- -- res <- mkReq $ PostOrder accId orderReq
      -- -- liftIO $ print res
      --   res <- mkReq $ GetAccountSummary accId
      --   liftIO $ print res
  print res
