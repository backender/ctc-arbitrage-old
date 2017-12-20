{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Markets (
  Market(..)
 , Volume
 , Price
 , Fees(..)
 , BookEntry(..)
 , Bid(..)
 , Ask(..)
 , GdaxOrderBook(..)
 , KrakenOrderBook(..)
 , GdaxBookEntry(..)
 , KrakenBookEntry(..)
 , OrderBook(..)
 , Order(..)
 , OrderType(..)
 , pollGdax
 , pollKraken
 , execute
) where


import GHC.Generics
import Data.Aeson
import Data.Aeson.Lens
import Data.Aeson.Types
import Data.Scientific
import Data.Either
import Data.Dynamic

import Control.Concurrent
import Control.Exception
import Control.Lens

import Network.Wreq
import qualified Network.HTTP.Client as HTTP
import qualified Network.HTTP.Client.TLS as HTTP

import Data.Time
import Data.Time.Clock.POSIX
import qualified Data.ByteString.Char8 as BS

import qualified Coinbase.Exchange.Types as CB
import qualified Coinbase.Exchange.Types.Core as CB
import qualified Coinbase.Exchange.Types.Private as CBPrivate
import qualified Coinbase.Exchange.Private as CBPrivate

import qualified Kraken as Kraken
import qualified Kraken.Request.AddOrder as Kraken

data Market = GDAX | Kraken deriving (Show, Eq)

type Volume = Double
type Price = Double
data Ask = Ask Market Integer Price Volume deriving Show
data Bid = Bid Market Integer Price Volume deriving Show


data KrakenBookEntry = KrakenBookEntry String String Integer deriving (Show, Generic) --Price Volume TimeStamp
data KrakenBids = KrakenBids [KrakenBookEntry] deriving (Show, Generic)
data KrakenAsks = KrakenAsks [KrakenBookEntry] deriving (Show, Generic)
data KrakenOrderBook = KrakenOrderBook KrakenBids KrakenAsks deriving (Show, Generic)

data GdaxBookEntry = GdaxBookEntry String String Double deriving (Show, Generic)
data GdaxAsks = GdaxAsks [GdaxBookEntry] deriving Show
data GdaxBids = GdaxBids [GdaxBookEntry] deriving Show
data GdaxOrderBook = GdaxOrderBook Integer GdaxBids GdaxAsks deriving (Show, Generic)

instance FromJSON KrakenBookEntry
instance FromJSON KrakenBids
instance FromJSON KrakenAsks
instance FromJSON KrakenOrderBook where
  parseJSON (Object v) =
    KrakenOrderBook <$>
      (v .: "bids") <*>
      (v .: "asks")

instance FromJSON GdaxBookEntry
instance FromJSON GdaxOrderBook where
  parseJSON (Object v) =
    GdaxOrderBook <$>
      (v .: "sequence") <*>
      ((v .: "bids") >>= fmap GdaxBids . parseJSON) <*>
      ((v .: "asks") >>= fmap GdaxAsks . parseJSON)


pollKraken :: MVar (Maybe KrakenOrderBook) -> IO ThreadId
pollKraken chan =
  forkIO $ do
    response <- try(get "https://api.kraken.com/0/public/Depth?pair=XBTUSD")
    case response of
      Left e -> do
        print (e :: SomeException)
        putMVar chan Nothing
      Right r ->
        case r ^? responseBody . key "result" . key "XXBTZUSD" of
          Nothing -> do
            print $ "Malicous response from Kraken: " ++ show r
            putMVar chan Nothing
          Just body -> do
            let book = parseMaybe parseJSON body :: Maybe KrakenOrderBook
            putMVar chan book

pollGdax :: MVar (Maybe GdaxOrderBook) -> IO ThreadId
pollGdax chan =
  forkIO $ do
    response <- try(get "https://api.gdax.com/products/BTC-USD/book")
    case response of
      Left e -> do
        print (e :: SomeException)
        putMVar chan Nothing
      Right r ->
        case r ^? responseBody of
          Nothing -> do
            print $ "Malicous response from Gdax: " ++ show r
            putMVar chan Nothing
          Just body -> do
            let book = decode body :: Maybe GdaxOrderBook
            putMVar chan book


--TODO: combine orderbook datatype and get rid of this type class
class OrderBook a where
  bestBid :: a -> Maybe Bid
  bestAsk :: a -> Maybe Ask
  bids :: a -> [Bid]
  asks :: a -> [Ask]

instance OrderBook GdaxOrderBook where
  bestBid book =
    case bids book of
      [] -> Nothing
      (x:xs) -> Just x
  bestAsk book =
    case asks book of
      [] -> Nothing
      (x:xs) -> Just x
  bids (GdaxOrderBook time (GdaxBids bids) _) =
    map (\(GdaxBookEntry price volume _) -> Bid GDAX time (read price) (read volume)) bids

  asks (GdaxOrderBook time _ (GdaxAsks asks)) =
    map (\(GdaxBookEntry price volume _) -> Ask GDAX time (read price) (read volume)) asks

instance OrderBook KrakenOrderBook where
  bids (KrakenOrderBook (KrakenBids bids) _) =
    map (\(KrakenBookEntry price volume time) -> Bid Kraken time (read price) (read volume)) bids
  asks (KrakenOrderBook _ (KrakenAsks asks)) =
    map (\(KrakenBookEntry price volume time) -> Ask Kraken time (read price) (read volume)) asks
  bestBid book =
    case bids book of
      [] -> Nothing
      (x:xs) -> Just x
  bestAsk book =
    case asks book of
      [] -> Nothing
      (x:xs) -> Just x


class BookEntry a where
  market :: a -> Market
  price :: a -> Price
  total :: a -> Price

instance BookEntry Ask where
  market (Ask m _ _ _) = m
  price (Ask _ _ p _) = p
  total (Ask _ _ p v) = p * v

instance BookEntry Bid where
  market (Bid m _ _ _) = m
  price (Bid _ _ p _) = p
  total (Bid _ _ p v) = p * v


class Fees a where
  makerFee :: a -> Price
  takerFee :: a -> Price

instance Fees Ask where
  makerFee ask@(Ask Kraken _ _ _) = 0.0016 * total ask
  makerFee ask@(Ask GDAX _ _ _) = 0.0
  takerFee ask@(Ask Kraken _ _ _) = 0.0026 * total ask
  takerFee ask@(Ask GDAX _ _ _) = 0.0025 * total ask

instance Fees Bid where
  makerFee bid@(Bid Kraken _ _ _) = 0.0016 * total bid
  makerFee bid@(Bid GDAX _ _ _) = 0.0
  takerFee bid@(Bid Kraken _ _ _) = 0.0026 * total bid
  takerFee bid@(Bid GDAX _ _ _) = 0.0025 * total bid


data OrderType = Buy | Sell deriving Show
data Order = Order OrderType Market Price Volume deriving Show


cbKey = ""
cbSecret = ""
cbPass = ""

krakKey = ""
krakPrivate = ""

cbConf = do
  manager <- HTTP.newManager HTTP.tlsManagerSettings
  let token = head $ rights [CB.mkToken cbKey cbSecret cbPass] -- TODO: handle
  return $ CB.ExchangeConf manager (Just token) CB.Live

cbProduct = CB.ProductId "BTC-USD"

executeCoinbase :: Order -> IO TradeInfo
executeCoinbase order@(Order s GDAX price amount) = do
  let side = case s of
              Buy -> CB.Buy
              Sell -> CB.Sell
  let p = CB.Price (CB.CoinScientific $ fromFloatDigits price)
  let v = CB.Size (CB.CoinScientific $ fromFloatDigits amount)
  let o = CBPrivate.NewLimitOrder cbProduct side CBPrivate.CancelBoth Nothing p v CBPrivate.FillOrKill False
  conf <- cbConf
  orderId <- CB.runExchange conf (CBPrivate.createOrder o)
  return $ CoinbaseTrade order (show orderId)


data TradeError = TradeError Order String deriving Show
instance Exception TradeError

data TradeInfo = KrakenTrade { kOrder :: Order, kResponse :: String }
               | CoinbaseTrade { cOrder :: Order, cOrderId :: String }
               deriving Show

executeKraken :: Order -> IO TradeInfo
executeKraken order@(Order s Kraken price amount) = do
  let side = case s of
            Buy -> "buy"
            Sell -> "sell"
  let o = Kraken.newLimitOrder side (show price) (show amount)
  response <- Kraken.private krakKey krakPrivate o :: IO Value
  case code response of
      200 -> return $ KrakenTrade order (show response)
      _ -> throw $ TradeError order (show response)
  where code r = 200 -- Extract status code out of Value

execute :: Order -> IO TradeInfo
execute order@(Order _ GDAX _ _) = do
  --executeCoinbase order
  let demoTrade = CoinbaseTrade order "demoId"
  print demoTrade
  return demoTrade
execute order@(Order _ Kraken _ _) = do
  --executeKraken order
  let demoTrade = KrakenTrade order "demoResponse"
  print demoTrade
  return demoTrade
