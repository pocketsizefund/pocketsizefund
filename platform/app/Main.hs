module Main (main) where
import Network.Wai.Handler.Warp (run, runSettings, defaultSettings, setLogger, setPort)
import Network.Wai.Logger (withStdoutLogger)

import Lib
import Network.Wai.Handler.Warp (defaultSettings, run, runSettings, setLogger, setPort)
import Network.Wai.Logger (withStdoutLogger)

main :: IO ()
main = withStdoutLogger $ \aplogger -> do
  let settings = setPort 8080 $ setLogger aplogger defaultSettings
  runSettings settings app

