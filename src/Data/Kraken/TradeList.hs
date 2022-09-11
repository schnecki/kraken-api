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
import qualified Data.HashMap.Strict     as HM (elems, filterWithKey, keys)
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
    withObject "Data.Kraken.TradeList" $ \o ->
      $(pureLogDebug) ("Data.Kraken.TradeList parseJSON input: " ++ show o) $ do
        -- last' <- (o .: "last") >>= parseStrToNum >>= parseJSON
        let oWoLast = HM.filterWithKey (\k _ -> k /= "last") o
        datas <- mapM (withArray "Data.Kraken.TradeList parsing Trades" $ fmap V.toList . mapM parseJSON) (HM.elems oWoLast)
        let tradeObjs = zipWith (\instr -> TradeObject instr . checkInstrData) (HM.keys oWoLast) datas
            lastComp | null (concatMap trades tradeObjs) = Nothing
                     | otherwise = Just . maximum $ concatMap (map (dateTimeToNanoSeconds . time) . trades) tradeObjs
        return $ TradeList lastComp tradeObjs

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
          $(pureLogPrintInfoText) ("Used data: " <> tshow (length acc) <> "/" <> tshow (length xs) <> ". Decision: " <> T.pack (show (timePositive, normalPriceChange expPrice newPriceValue, normalTimeStep expDiffTime newDiffTime)))
          $(pureLogPrintInfoText) ("newDiffTime <= 50 * expTimeDiff: " <> T.pack  (show (newDiffTime, 50 * expDiffTime)) <> ", Calc: " <> T.pack (show(time x , time (last acc), dateTimeToNanoSeconds (time x) , dateTimeToNanoSeconds (time $ last acc))))
          acc
      where
        newPriceValue = priceValueToDouble $ price x
        newDiffTime = fromIntegral $ max minDiffTime $ abs $ dateTimeToNanoSeconds (time x) - dateTimeToNanoSeconds (time $ last acc)
        timePositive = dateTimeToNanoSeconds (time x) > dateTimeToNanoSeconds (time $ last acc) && dateTimeToNanoSeconds (time x) > dateTimeToNanoSeconds (time first)
        newExpPrice = (1 - alpha) * expPrice + alpha * priceValueToDouble (price x)
        newExpDiffTime = (1 - alpha) * expDiffTime + alpha * newDiffTime
    minDiffTime = 1 * 10^8
    normalPriceChange expPrice val = val >= 0.1 * expPrice && val <= 10 * expPrice
    normalTimeStep expTimeDiff newDiffTime = newDiffTime <= 50 * expTimeDiff || newDiffTime < 3 * 1e9
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
--       mkKeys o = filter (/= "last") (HM.keys o)
--       mkElems o = HM.elems $ HM.filterWithKey (\k _ -> k /= "last") o


prettyTradeList :: TradeList -> Doc
prettyTradeList (TradeList last' xs) =
  mVal last' (\v -> colName "Trade Last ID" $$ nest nestCols (integer v)) $+$
  vcat (map prettyTradeObject xs)
