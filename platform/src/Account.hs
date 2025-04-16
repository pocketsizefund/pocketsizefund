{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass #-}

module Account (Account(..)) where

import GHC.Generics (Generic)
import Data.Aeson
import Data.Fixed (Centi)
import Text.Read (readMaybe)

type Money = Centi

data Currency = USD
  deriving (Show, Read, Eq, Generic, FromJSON, ToJSON)

data AdminConfigurations = AdminConfigurations
  {
    allow_instant_ach :: Bool
  } deriving (Show, Generic, FromJSON, ToJSON)

data Account = Account
  { id :: String
  , account_number :: String
  , status :: String
  , user_configurations :: Maybe String
  , crypto_status :: String
  , admin_configurations :: AdminConfigurations
  , currency :: String
  , cash :: String
  , trade_suspended_by_user :: Bool
  , daytrade_count :: Int
  } deriving (Show, Generic, FromJSON, ToJSON)

