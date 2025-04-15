{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}
{-# LANGUAGE OverloadedStrings #-}
module Lib
    (
    app
    ) where

import Data.Aeson
import Data.Aeson.TH
import Data.Text (Text)
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import System.Environment (getEnv)



data User = User
  { userId        :: Int
  , userFirstName :: String
  , userLastName  :: String
  } deriving (Eq, Show)

$(deriveJSON defaultOptions ''User)

type API = "health" :> Get '[PlainText] Text

app :: Application
app = serve api server

api :: Proxy API
api = Proxy

server :: Server API
server = return "OK"

getApiKey :: IO String
getApiKey = getEnv "ALPACA_API_KEY"
