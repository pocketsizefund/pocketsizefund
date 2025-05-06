{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Lib
    ( startApplication
    ) where

import Control.Exception (bracket)
import Control.Monad (forM)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson
import Data.Int (Int32)
import Data.Text (Text, pack, unpack)
import Data.Time (UTCTime, defaultTimeLocale, parseTimeM)
import Database.DuckDB (open, connect, closeDatabase, closeConnection, runDuckDB, prepare, bindVarChar, executePrepared, valueVarChar, valueDouble, valueInt32, rowCount, destroy, destroyPrepare)
import Database.DuckDB.Internal.FFI (DuckDBDatabase, DuckDBConnection)
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import System.Environment (getEnv)
import EquityBar
import System.Directory (doesFileExist, getPermissions, readable) -- TEMP

type API = "bars" :> Capture "ticker" Text :> Get '[JSON] [EquityBar]

server :: DuckDBDatabase -> DuckDBConnection -> Server API
server db conn = getEquityBars
  where
    getEquityBars :: Text -> Handler [EquityBar]
    getEquityBars ticker = liftIO $ queryEquityBars db conn ticker

queryEquityBars :: DuckDBDatabase -> DuckDBConnection -> Text -> IO [EquityBar]
queryEquityBars db conn ticker = do
  eitherResult <- runDuckDB $ do
    stmt <- prepare conn "SELECT ticker, timestamp, open_price, high_price, low_price, close_price, trades, volume, volume_weighted_average_price FROM equity_bars WHERE ticker = ?"
    bindVarChar stmt 1 (unpack ticker)
    result <- executePrepared stmt
    rows <- rowCount result
    bars <- forM [0 .. rows - 1] $ \row -> do
      ticker' <- pack <$> valueVarChar result 0 row
      timestampStr <- valueVarChar result 1 row
      timestamp <- case parseTimeM True defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ" timestampStr of
        Just ts -> pure ts
        Nothing -> throwError $ "Invalid timestamp format: " ++ timestampStr
      open_price <- valueDouble result 2 row
      high_price <- valueDouble result 3 row
      low_price <- valueDouble result 4 row
      close_price <- valueDouble result 5 row
      trades <- fromIntegral <$> valueInt32 result 6 row
      volume <- fromIntegral <$> valueInt32 result 7 row
      volume_weighted_average_price <- valueDouble result 8 row
      pure EquityBar {..}
    destroy result
    destroyPrepare stmt
    pure bars
  case eitherResult of
    Left err -> do
      putStrLn $ "DuckDB error: " ++ err
      pure []
    Right bars -> pure bars

startApplication :: IO ()
startApplication = do
  dbPath <- getEnv "DUCKDB_PATH"
  exists <- doesFileExist dbPath -- TEMP
  print exists -- TEMP
  if exists -- TEMP
    then do
        perms <- getPermissions dbPath
        putStrLn $ "Permissions: " ++ show perms ++ ", Readable: " ++ show (readable perms)
        putStrLn $ "Attempting to open DuckDB database at: " ++ dbPath
    else do
        putStrLn $ "Database file does not exist at: " ++ dbPath
  putStrLn $ "Using DuckDB database at: " ++ dbPath -- TEMP
  bracket
    (do
      eitherDb <- runDuckDB $ open dbPath
      case eitherDb of
        Left err -> error $ "Failed to open database: " ++ show err
        Right db -> do
          eitherConn <- runDuckDB $ connect db
          case eitherConn of
            Left err -> do
              runDuckDB $ closeDatabase db
              error $ "Failed to connect: " ++ err
            Right conn -> pure (db, conn))
    (\(db, conn) -> runDuckDB $ do
      closeConnection conn
      closeDatabase db)
    (\(db, conn) -> run 8080 $ application db conn)

application :: DuckDBDatabase -> DuckDBConnection -> Application
application db conn = serve api $ server db conn

api :: Proxy API
api = Proxy
