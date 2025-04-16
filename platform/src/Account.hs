{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Account (Account (..)) where

import Data.Aeson
import GHC.Generics (Generic)
import Text.Read (readMaybe)

newtype AdminConfigurations = AdminConfigurations {allow_instant_ach :: Bool}
  deriving (Show, Generic, FromJSON, ToJSON)

data Account = Account
  { id :: String,
    account_number :: String,
    status :: String,
    user_configurations :: Maybe String,
    crypto_status :: String,
    admin_configurations :: AdminConfigurations,
    currency :: String,
    cash :: String,
    trade_suspended_by_user :: Bool,
    daytrade_count :: Int
  }
  deriving (Show, Generic, FromJSON, ToJSON)
