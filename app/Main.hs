{-# LANGUAGE OverloadedStrings #-}

import Control.Concurrent
import Control.Exception
import Control.Monad

import Data.Maybe
import Data.List
import Data.Time

import Markets
import System.IO


import Data.Aeson

--import Debug.Trace

-- If async is False skips additional check of whether the previous trade
-- was executed.
asyncTrading = False
-- Api request limit (millisec) for polling the orderbook.
-- Currently Kraken does not handle more than 3 requests/second (needs 0.33s delay).
orderbookRequestLimit = 400000
-- Minimal net profit required in order to execute trade.
minProfitMargin = 0.0


comp :: Ord a => a -> a -> Ordering
comp p p'
  | p < p' = LT
  | p > p' = GT
  | otherwise = EQ

comparePrice :: BookEntry a => a -> a -> Ordering
comparePrice a b = comp (price a) (price b)

minVolume :: Bid -> Ask -> Double
minVolume bid@(Bid _ _ _ v) ask@(Ask _ _ _ v') = min v v'

tradeFee' :: Bid -> Ask -> (Double, Double)
tradeFee' bid@(Bid m t p v) ask@(Ask m' t' p' v') = do
  let minV = minVolume bid ask
  let bidFee = takerFee $ Bid m t p minV
  let askFee = takerFee $ Ask m' t' p' minV
  (bidFee, askFee)

tradeFee :: Bid -> Ask -> Double
tradeFee bid ask = do
  let (bidFee, askFee) = tradeFee' bid ask
  bidFee + askFee

spread :: Bid -> Ask -> Double
spread (Bid _ _ p _) (Ask _ _ p' _) = p - p'

spreadPc :: Bid -> Ask -> Double
spreadPc (Bid _ _ p _) (Ask _ _ p' _) = 100 / p' * p - 100

profit :: Bid -> Ask -> Double
profit bid ask = (spread bid ask) * (minVolume bid ask)

profitIncl :: Bid -> Ask -> Double
profitIncl bid ask = profit bid ask - tradeFee bid ask

logTradeInfo :: [Bid] -> [Ask] -> IO ()
logTradeInfo bids asks = do
  let maxBid = maximumBy comparePrice bids
  let minAsk = minimumBy comparePrice asks
  putStr "\n\n"
  putStrLn $ "[BID] Max: " ++ show maxBid
  putStrLn $ "[ASK] Min: " ++ show minAsk
  putStrLn "--------------------------------------------------"
  putStrLn $ "Spread ($): " ++ show (spread maxBid minAsk)
  putStrLn $ "Spread (%): " ++ show (spreadPc maxBid minAsk)
  putStrLn $ "Tradable Volume (฿): " ++ show (minVolume maxBid minAsk)
  putStrLn $ "Fee Markets $: " ++ show (tradeFee' maxBid minAsk)
  putStrLn $ "Fees ($): " ++ show (tradeFee maxBid minAsk)
  putStrLn $ "Profit ecl. Fee ($): " ++ show (profit maxBid minAsk)
  putStrLn $ "Profit incl. Fee ($): " ++ show (profitIncl maxBid minAsk)
  hFlush stdout


trade :: [Bid] -> [Ask] -> IO ()
trade [] [] = print "Neither bids nor asks available"
trade [] asks = print "No bids available"
trade bids [] = print "No asks available"
trade bids asks = do
  -- Strategy: which Bid and Ask to be used!
  let maxBid = maximumBy comparePrice bids
  let minAsk = minimumBy comparePrice asks
  --print . maximum $ map (\(b, a) -> spread b a) [(b, a) | b <- bids, a <- asks]
  --
  when (market maxBid /= market minAsk) $ do
    logTradeInfo bids asks
    when (profitIncl maxBid minAsk > minProfitMargin) $ do
      let vol = (minVolume maxBid minAsk)
      let buy = Order Buy (market minAsk) (price minAsk) vol
      let sell = Order Sell (market maxBid) (price maxBid) vol
      print buy
      print sell
      putStrLn "==================================================="
      trades <- mapM execute [buy, sell]
      putStrLn "==================================================="
      hFlush stdout


tradeThreashold :: UTCTime -> UTCTime -> Int
tradeThreashold start end = do
  let executionTime = ceiling $ (realToFrac (diffUTCTime end start) :: Double)*1000000
  max 0 (orderbookRequestLimit - executionTime)

delay :: UTCTime -> UTCTime -> IO ()
delay start end =
  when (tradeThreashold > 0) $ do
    putStrLn $ "\nWait: " ++ show tradeThreashold ++ "\n"
    threadDelay tradeThreashold
  where executionTime = ceiling $ (realToFrac (diffUTCTime end start) :: Double)*1000000
        tradeThreashold = max 0 (orderbookRequestLimit - executionTime)


kakey = ""
kpkey = ""

main :: IO ()
main = do

  kraken <- newEmptyMVar
  gdax <- newEmptyMVar

  forever $ do
    start <- getCurrentTime

    _ <- pollKraken kraken
    _ <- pollGdax gdax
    gdax' <- takeMVar gdax
    kraken' <- takeMVar kraken

    let bids = (catMaybes . catMaybes) [bestBid <$> gdax', bestBid <$> kraken']
    let asks = (catMaybes . catMaybes) [bestAsk <$> gdax', bestAsk <$> kraken']
    trade bids asks

    let (Ask _ _ pa _) = (head . catMaybes . catMaybes) $ [bestAsk <$> kraken']
    let (Bid _ _ pb _) = (head . catMaybes . catMaybes) $ [bestBid <$> kraken']
    -- Kraken: sell 0.001 BTC
    -- let order = Kraken.newLimitOrder "sell" (show $ pa) "0.001"
    -- b <- Kraken.private kakey kpkey order :: IO Value
    -- print b
    -- Kraken: sell 0.001 BTC
    -- let order = Kraken.newLimitOrder "buy" (show $ pb) "0.001"
    -- b <- Kraken.private kakey kpkey order :: IO Value
    -- print b

    --execute (Order Sell GDAX pa 0.01)

    end <- getCurrentTime

    delay start end
