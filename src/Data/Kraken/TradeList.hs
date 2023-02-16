{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Data.Kraken.TradeList
    ( TradeList(..)
    , prettyTradeList
    , allTrades
    ) where

import           Control.DeepSeq
import           Data.Aeson
import           Data.Aeson.Key
import qualified Data.Aeson.KeyMap       as HM
import           Data.List
import           Data.Serialize
import qualified Data.Text               as T
import qualified Data.Vector             as V
import           EasyLogger
import           GHC.Generics
import           Prelude
import           Text.PrettyPrint        hiding ((<>))

import           Data.Kraken.DateTime
import           Data.Kraken.PriceValue
import           Data.Kraken.Trade
import           Data.Kraken.TradeObject
import           Data.Kraken.Util

data TradeList =
  TradeList
    { tradeLast :: Maybe Integer
    , tradeList :: [TradeObject]
    }
  deriving (Read, Show, Eq, Ord, Generic, NFData, Serialize)

allTrades :: TradeList -> [Trade]
allTrades = concatMap trades . tradeList


instance FromJSON TradeList where
  parseJSON =
    withObject "Data.Kraken.TradeList" $ \o -> do
    -- last' <- (o .: "last") >>= parseStrToNum >>= parseJSON
      let oWoLast = HM.filterWithKey (\k _ -> k /= "last") o
      datas <- mapM (withArray "Data.Kraken.TradeList parsing Trades" $ fmap V.toList . mapM parseJSON) (HM.elems oWoLast)
      let tradeObjs = zipWith (\instr -> TradeObject (toText instr) . checkInstrData) (HM.keys oWoLast) datas
          nanoSecs = concatMap (map (dateTimeToNanoSeconds . time) . trades) tradeObjs
          lastComp
            | null nanoSecs = Nothing
            | otherwise = Just . maximum $ nanoSecs -- concatMap (map (dateTimeToNanoSeconds . time) . trades) tradeObjs
      return $
        $(pureLogPrintInfoText)
          ("End of parsing. After filtering: " <>
           tshow (sum $ map (length . trades) tradeObjs) <> "/" <> tshow (sum $ map length datas) <> ". tradeLast: " <> tshow (nanoSecondsToDateTime <$> lastComp)) $
        TradeList lastComp tradeObjs

-- | During testing we found a Bug at the Kraken API. Sometimes Kraken seems to send wrong trades/data from other coins with other datetimes after some period of time. Hence, for security reasons, we
-- rather perform a check here!
checkInstrData :: [Trade] -> [Trade]
checkInstrData [] = []
checkInstrData [x] = [x]
checkInstrData xs@(first:_) = checkDataPoint startStats startXs restXs
  where
    checkDataPoint :: (Double, Double) -> [Trade] -> [Trade] -> [Trade]
    checkDataPoint _ acc [] = acc
    checkDataPoint (expPrice, expDiffTime) acc (x:xs')
      | timePositive && normalPriceChange expPrice newPriceValue && normalTimeStep expDiffTime newDiffTime = checkDataPoint (newExpPrice, newExpDiffTime) (acc ++ [x]) xs'
      | otherwise =
          $(pureLogPrintInfoText) ("Rejecting data. Used data: " <> tshow (length acc) <> "/" <> tshow (length xs) <> ". Time pos, Norm delta price, Normal time step: " <>
                                   T.pack (show (timePositive, normalPriceChange expPrice newPriceValue, normalTimeStep expDiffTime newDiffTime)))
          $(pureLogPrintInfoText) (if timePositive then "" else "Last data point: " <> tshow (time $ last acc) <> ". Rejecting data from: " <> tshow (time x))
          $(pureLogPrintInfoText) (if normalPriceChange expPrice newPriceValue then "" else ("Price new, Price Last: " <> tshow (newPriceValue, priceValueToDouble $ price (last acc))))
          $(pureLogPrintInfoText) (if normalTimeStep expDiffTime newDiffTime then "" else
                                     "newDiffTime <= 50 * expTimeDiff: " <> T.pack  (show (newDiffTime, 50 * expDiffTime)) <> ", (Time new, Time last): " <> T.pack (show (time x, time (last acc))))

          acc
      where
        newPriceValue = priceValueToDouble $ price x
        newDiffTime = fromIntegral $ max minDiffTime $ abs $ dateTimeToNanoSeconds (time x) - dateTimeToNanoSeconds (time $ last acc)
        timePositive = dateTimeToNanoSeconds (time x) > dateTimeToNanoSeconds (time $ last acc) && dateTimeToNanoSeconds (time x) > dateTimeToNanoSeconds (time first)
        newExpPrice = (1 - alpha) * expPrice + alpha * priceValueToDouble (price x)
        newExpDiffTime = (1 - alpha) * expDiffTime + alpha * newDiffTime
    minDiffTime = 10*10^9
    normalPriceChange expPrice val = val >= 0.1 * expPrice && val <= 10 * expPrice
    normalTimeStep expTimeDiff newDiffTime = newDiffTime <= 100 * expTimeDiff || newDiffTime < 30 * 60 * 10^9
    alpha = 0.05
    (startXs, restXs) = splitAt 10 xs
    startStats = (startPrice, startDiffTime)
    startPrice = sum (map (priceValueToDouble . price) startXs) / max 1 (fromIntegral $ length startXs)
    startDiffTime = sum diffTimes / max 1 (fromIntegral $ length diffTimes)
      where
        diffTimes = zipWith (\x y -> fromIntegral $ max minDiffTime $ abs $ dateTimeToNanoSeconds (time x) - dateTimeToNanoSeconds (time y)) (tail startXs) (init startXs)


-- instance FromJSON TradeList where
--   parseJSON =
--     withObject "Data.Kraken.TradeList" $ \o -> do
--       last' <- parseJSON =<< parseStrToNum =<< (o .: "last")
--       let oHM = toHashMapText o
--       datas <- mapM (withArray "Data.Kraken.TradeList parsing Trades" $ \o' -> V.toList <$> mapM parseJSON o') (mkElems oHM)
--       return $ TradeList last' $ zipWith TradeObject (mkKeys oHM) datas
--     where
--       mkKeys o = filter (/= "last") (map toText . HM.keys $ o)
--       mkElems o = HM.elems $ HM.filterWithKey (\k _ -> k /= "last") o


prettyTradeList :: TradeList -> Doc
prettyTradeList (TradeList last' xs) =
  mVal last' (\v -> colName "Trade Last ID" $$ nest nestCols (integer v)) $+$
  vcat (map prettyTradeObject xs)
