{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module EquityBar
    ( EquityBar(..)
    , EquityBarsResponse(..)
    , convertEquityBarResponseToEquityBar
    , getEquityBarsByTicker
    ) where

import Data.Aeson
import Data.Aeson.Types (Parser, parse)
import Data.Text (Text, pack, unpack)
import Data.Time (UTCTime, parseTimeM, defaultTimeLocale, formatTime)
import GHC.Generics
import Control.Monad (forM)
import Data.List (find)
import qualified Data.Vector as Vector
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Aeson.Key (toString)

data EquityBar = EquityBar {
    ticker :: Text
  , timestamp :: UTCTime
  , open_price :: Double
  , high_price :: Double
  , low_price :: Double
  , close_price :: Double
  , trades :: Int
  , volume :: Int
  , volume_weighted_average_price :: Double
} deriving (Show, Eq, Generic)

instance FromJSON EquityBar where 
  parseJSON = withObject "EquityBar" $ \v -> do
    ticker <- v .: "ticker"
    timestampText <- v .: "timestamp"
    timestamp <- case parseTimeM True defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ" (unpack timestampText) of
      Just ts -> pure ts
      Nothing -> fail "Invalid timestamp format"
    open_price <- v .: "open_price"
    high_price <- v .: "high_price"
    low_price <- v .: "low_price"
    close_price <- v .: "close_price"
    trades <- v .: "trades"
    volume <- v .: "volume"
    volume_weighted_average_price <- v .: "volume_weighted_average_price"
    pure $ EquityBar 
      {
        ticker = ticker
      , timestamp = timestamp
      , open_price = open_price
      , high_price = high_price
      , low_price = low_price
      , close_price = close_price
      , trades = trades
      , volume = volume
      , volume_weighted_average_price = volume_weighted_average_price
      }

instance ToJSON EquityBar where
  toJSON (EquityBar ticker timestamp open_price high_price low_price close_price trades volume volume_weighted_average_price) =
    object [ "ticker" .= ticker
           , "timestamp" .= formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ" timestamp
           , "open_price" .= open_price
           , "high_price" .= high_price
           , "low_price" .= low_price
           , "close_price" .= close_price
           , "trades" .= trades
           , "volume" .= volume
           , "volume_weighted_average_price" .= volume_weighted_average_price
           ]

convertEquityBarResponseToEquityBar :: Text -> Value -> Either String EquityBar
convertEquityBarResponseToEquityBar ticker val = case parse parser val of
  Success bar -> Right bar
  Error err -> Left $ "Failed to parse EquityBar: " ++ err
  where
    parser :: Value -> Parser EquityBar
    parser = withObject "EquityBar" $ \v -> do
      timestampText <- v .: "t"
      timestamp <- case parseTimeM True defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ" (unpack timestampText) of
        Just ts -> pure ts
        Nothing -> fail "Invalid timestamp format"  
      open_price <- v .: "o"
      high_price <- v .: "h"
      low_price <- v .: "l"
      close_price <- v .: "c"
      trades <- v .: "n"
      volume <- v .: "v"
      volume_weighted_average_price <- v .: "vw"
      pure $ EquityBar
        { ticker = ticker
        , timestamp = timestamp
        , open_price = open_price
        , high_price = high_price
        , low_price = low_price
        , close_price = close_price
        , trades = trades
        , volume = volume
        , volume_weighted_average_price = volume_weighted_average_price
        }

data EquityBarsResponse = EquityBarsResponse
  { bars :: [(Text, [EquityBar])]
  , next_page_token :: Maybe Text
  } deriving (Show, Generic)

instance FromJSON EquityBarsResponse where
  parseJSON = withObject "EquityBarsResponse" $ \v -> do
    barsObj <- v .: "bars"
    barsList <- forM (objectPairs barsObj) $ \(ticker, barsVal) ->
      case barsVal of
        Array arr -> do
          bars <- forM (Vector.toList arr) $ \barVal ->
            case convertEquityBarResponseToEquityBar ticker barVal of
              Right bar -> pure bar
              Left err -> fail err
          pure (ticker, bars)
        _ -> fail "Expected an array of equity bars"
    nextPage <- v .: "next_page_token"
    pure $ EquityBarsResponse
      { bars = barsList
      , next_page_token = nextPage
      }
    where
      objectPairs :: Object -> [(Text, Value)]
      objectPairs = map (\(k, v) -> (pack (toString k), v)) . KeyMap.toAscList

getEquityBarsByTicker :: Text -> EquityBarsResponse -> [EquityBar]
getEquityBarsByTicker ticker response =
  maybe [] snd (find (\ (t, _) -> t == ticker) (bars response))
