module Main (main) where

import Lib
import Network.Wai.Handler.Warp (defaultSettings, runSettings, setLogger, setPort)
import Network.Wai.Logger (withStdoutLogger)

main :: IO ()
main = withStdoutLogger $ \aplogger -> do
  let settings = setPort 8080 $ setLogger aplogger defaultSettings
  runSettings settings app
