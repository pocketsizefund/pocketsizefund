module Main where

import Test.Hspec

main :: IO ()
main = hspect $ do
    describe "DataManager" $ do
        it "is a placeholder test" $ do
            1 + 1 `shouldBe` 2
