{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Lib
  ( app,
  )
where

import Account
import Control.Lens
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (eitherDecode)
import Data.ByteString.Char8 (pack)
import qualified Data.ByteString.Lazy.Char8 as BL8
import Network.Wai
import Network.Wreq (defaults, getWith, header, responseBody)
import Servant
import System.Environment (getEnv)

type API =
  "health" :> Get '[JSON] NoContent
    :<|> "account" :> Get '[JSON] Account

app :: Application
app = serve api server

api :: Proxy API
api = Proxy

server :: Server API
server =
  pure NoContent
    :<|> getAccount

getAccount :: Handler Account
getAccount = do
  key <- liftIO $ getEnv "ALPACA_API_KEY"
  secret <- liftIO $ getEnv "ALPACA_API_SECRET"
  baseUrl <- liftIO $ getEnv "ALPACA_BASE_URL"

  let opts =
        defaults
          & header "APCA-API-KEY-ID" .~ [pack key]
          & header "APCA-API-SECRET-KEY" .~ [pack secret]
      url = baseUrl ++ "/v2/account"

  response <- liftIO $ getWith opts url
  let body = response ^. responseBody
  case eitherDecode body of
    Right account -> return account
    Left err -> do
      liftIO $ putStrLn "Error decoding account:"
      liftIO $ print err
      liftIO $ putStrLn $ "Raw response: " ++ BL8.unpack body
      throwError $ err404 {errBody = "Alpaca account not found or unparseable"}
