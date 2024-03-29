{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Main where

import           ApiMaker
import           Control.Monad                      (forM, forM_, void)
import           Control.Monad.Except
import           Control.Monad.Trans                (liftIO)
import           Control.Monad.Trans.State
import           Data.Aeson                         (encode)
import qualified Data.ByteString                    as B
import qualified Data.ByteString.Char8              as C
import           Data.Maybe                         (isJust)
import qualified Data.Text                          as T
import           Data.Time.Clock
import qualified Data.Word                          as W8
import           EasyLogger
import           Prelude                            hiding (id)

import           Data.Kraken.AccountBalanceList
import           Data.Kraken.AssetInfoList
import           Data.Kraken.CandlestickGranularity
import           Data.Kraken.ClosedOrderList
import           Data.Kraken.DateTime
import           Data.Kraken.OpenOrderList
import           Data.Kraken.OrderAdded
import           Data.Kraken.OrderBookList
import           Data.Kraken.OrderType
import           Data.Kraken.PositionList
import           Data.Kraken.ServerTime
import           Data.Kraken.SpreadList
import           Data.Kraken.SpreadObject
import           Data.Kraken.SystemStatus
import           Data.Kraken.TickDataList
import           Data.Kraken.TickDataObject
import           Data.Kraken.TickerInformationList
import           Data.Kraken.TradableAssetPairList
import           Data.Kraken.TradeBalance
import           Data.Kraken.TradeHistoryList
import           Data.Kraken.TradeInfo
import           Data.Kraken.TradeInfoList
import           Data.Kraken.TradeInfoObject
import           Data.Kraken.TradeList
import           Data.Kraken.TradeObject
import           KrakenApi

main :: IO ()
main = do
  $(initLoggerAllPackages) (LogFile "kraken-api.log") True
  enableKrakenApiLogging (LogFile "kraken-api.log")
  enableRequestLogging (LogFile "kraken-api.log")
  setMinLogLevel LogDebug -- Levels: LogNone < LogAll < LogDebug < LogInfo < LogWarning < LogError
  setPrintLocationToConsole True
  $(logInfo) ("Starting App" :: T.Text)

  -- Starting
  apiKey <- C.filter (/= '\n') <$> B.readFile "API_KEY"
  putStrLn $ "API_KEY: " <> show apiKey
  prApiKey <- C.filter (/= '\n') <$> B.readFile "API_KEY_PRIVATE"
  putStrLn $ "API_KEY_PRIVATE: " <> show prApiKey
  let cfg = krakenConfigTradeAccount apiKey prApiKey Nothing
      lastX x = reverse . take x . reverse
  res <-
    runSessReqWithParamsM (additionalParams cfg) cfg $
    runRequests $ do
      -- res <- mkReq GetServerTime
      -- liftIO $ print $ prettyServerTime res
      -- res <- mkReq GetSystemStatus
      -- liftIO $ print $ prettySystemStatus res
      -- res <- mkReq $ GetAssetInfo "INJ,ADA" Nothing
      -- liftIO $ print $ prettyAssetInfoList res
      -- res <- mkReq $ GetTradableAssetPairs (TradableAssetPairsConfig "ADAEUR" (Just Info))
      -- liftIO $ print $ prettyTradableAssetPairList res

      -- res <- mkReq $ GetTickerInformation "ADAEUR"
      -- liftIO $ print $ prettyTickerInformationList res
      -- res@(TickDataList gr l dats) <- mkReq $ GetOHLCData (OHLCDataConfig "ADAEUR" (Just H4) (Just 1633515697))
      -- liftIO $ print $ prettyTickDataList $ TickDataList gr l (map (\x -> x {tickData = take 2 (tickData x) ++ lastX 4 (tickData x)}) dats)
      -- liftIO $ putStrLn $ "Number TickData: " ++ show (map (length . tickData) dats)
      -- res <- mkReq $ GetOrderBook (OrderBookConfig "ADAEUR" (Just 4))
      -- liftIO $ print $ prettyOrderBookList res
      -- TradeList l' res <- mkReq $ GetTrades (TradesConfig "ADAEUR" Nothing)
      -- liftIO $ print $ prettyTradeList $ TradeList l' (map (\x -> x { Data.Kraken.TradeObject.trades = take 2 (Data.Kraken.TradeObject.trades x) ++ lastX 3 (Data.Kraken.TradeObject.trades x)}) res)
      -- SpreadList l' res <- mkReq $ GetRecentSpreads (RecentSpreadsConfig "ADAEUR" Nothing)
      -- liftIO $ print $ prettySpreadList $ SpreadList l' (map (\x -> x { spreads = take 2 (spreads x) ++ lastX 3 (spreads x)}) res)


      -- accBalLs <- mkReq GetAccountBalance
      -- liftIO $ print $ prettyAccountBalanceList accBalLs
      -- trBal <- mkReq $ GetTradeBalance (Just "ZUSD")
      -- liftIO $ print $ prettyTradeBalance trBal
      -- opOrds <- mkReq $ GetOpenOrders (OpenOrdersConfig Nothing Nothing)
      -- liftIO $ print $ prettyOpenOrderList opOrds
      -- clOrds <- mkReq $ GetClosedOrders (ClosedOrdersConfig Nothing Nothing Nothing Nothing Nothing Nothing)
      -- liftIO $ print $ prettyClosedOrderList clOrds
      -- -- trInfos <- mkReq $ QueryTradesInfo (QueryTradesInfoConfig "TNFTNS-IHEIE-3NVRNU" (Just True))
      -- -- liftIO $ print $ prettyTradeInfoList trInfos
      positions <- mkReq $ GetOpenPositions (OpenPositionsConfig Nothing True Nothing)
      liftIO $ print $ prettyPositionList positions


      TradeHistoryList res _ <- mkReq $ GetTradesHistory (TradesHistoryConfig Nothing (Just False) Nothing Nothing Nothing)
      let res' = filter (isJust . posstatus . tradeInfo) res
      liftIO $ print $ prettyTradeHistoryList $ TradeHistoryList res' (length res')


      res <- mkReq $ AddOrder $ AddOrderConfig Nothing AddOrderMarket Buy "0.1" "XXBTZUSD" Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing (Just AddOrderStopLoss) (Just "20000") Nothing Nothing (Just True)
      liftIO $ print $ prettyOrderAdded res

      -- clOrds <- mkReq $ GetClosedOrders (ClosedOrdersConfig Nothing Nothing Nothing Nothing Nothing Nothing)
      -- liftIO $ print $ prettyClosedOrderList $ ClosedOrderList (take 2 $ closedOrders clOrds)
      -- clOrds <- mkReq $ GetClosedOrders (ClosedOrdersConfig Nothing Nothing Nothing Nothing Nothing Nothing)
      -- liftIO $ print $ prettyClosedOrderList $ ClosedOrderList (take 2 $ closedOrders clOrds)
  print res
  finalizeAllLoggers
