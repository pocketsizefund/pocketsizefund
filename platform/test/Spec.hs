{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Lib (app)
import Test.Hspec
import Test.Hspec.Wai
import Text.RawString.QQ

main :: IO ()
main = hspec spec

spec :: Spec
spec = with (return app) $ do
  describe "GET /health" $ do
    it "responds with 200" $ do
      get "/health" `shouldRespondWith` 200

  describe "GET /account" $ do
    it "responds with 200" $ do
      get "/account" `shouldRespondWith` 200
