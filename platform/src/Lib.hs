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
import Data.Aeson
import Data.Aeson.TH
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Text (Text)
import Network.Wai
import Network.Wai.Handler.Warp
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
          & header "APCA-API-KEY-ID" .~ [BS.pack key]
          & header "APCA-API-SECRET-KEY" .~ [BS.pack secret]
      url = baseUrl ++ "/v2/account"

  r <- liftIO $ getWith opts url
  case Data.Aeson.decode (r ^. responseBody) of
    Just account -> return account
    Nothing -> throwError $ err404 {errBody = "Alpaca account not found or unparseable"}
