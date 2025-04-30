{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Account (Account (..)) where

import Data.Aeson (FromJSON, ToJSON, Value (String), defaultOptions, fieldLabelModifier, genericParseJSON, parseJSON, toJSON)
import GHC.Generics (Generic)

newtype AdminConfigurations = AdminConfigurations {allowInstantAch :: Bool}
  deriving (Show, Generic, ToJSON)

instance FromJSON AdminConfigurations where
  parseJSON =
    genericParseJSON
      defaultOptions
        { fieldLabelModifier = \case
            "allowInstantAch" -> "allow_instant_ach"
            x -> x
        }

data Status
  = ACTIVE
  | ONBOARDING
  | SUBMITTED
  | SUBMISSION_FAILED
  | ACCOUNT_UPDATED
  | APPROVAL_PENDING
  | REJECTED
  deriving (Show, Generic, Enum, FromJSON, ToJSON)

data CryptoStatus = PAPER_ONLY deriving (Show, Generic, Enum)

instance FromJSON CryptoStatus where
  parseJSON (String "PAPER_ONLY") = pure PAPER_ONLY
  parseJSON _ = fail "Invalid CryptoStatus"

instance ToJSON CryptoStatus where
  toJSON PAPER_ONLY = String "PAPER_ONLY"

data Multiplier
  = CashAccount
  | MarginAccount
  | PatternDayTrader
  deriving (Show, Eq, Generic, Enum, Bounded)

instance FromJSON Multiplier where
  parseJSON (String n) = case n of
    "1" -> pure CashAccount
    "2" -> pure MarginAccount
    "4" -> pure PatternDayTrader
    _ -> fail "Invalid Multiplier value"
  parseJSON _ = fail "Multiplier must be a number"

instance ToJSON Multiplier where
  toJSON CashAccount = String "cashAccount"
  toJSON MarginAccount = String "marginAccount"
  toJSON PatternDayTrader = String "patternDayTrader"

data Account = Account
  { id_ :: String,
    adminConfigurations :: AdminConfigurations,
    userConfigurations :: Maybe String,
    accountNumber :: String,
    status :: Status,
    cryptoStatus :: CryptoStatus,
    currency :: String,
    buyingPower :: String,
    regulationBuyingPower :: String,
    daytradingBuyingPower :: String,
    effectiveBuyingPower :: String,
    nonMarginableBuyingPower :: String,
    beginningOfDayBuyingPower :: String,
    cash :: String,
    accruedFees :: String,
    portfolioValue :: String,
    patternDayTrader :: Bool,
    tradingBlocked :: Bool,
    transfersBlocked :: Bool,
    accountBlocked :: Bool,
    createdAt :: String,
    tradeSuspendedByUser :: Bool,
    multiplier :: Multiplier,
    shortingEnabled :: Bool,
    equity :: String,
    lastEquity :: String,
    longMarketValue :: String,
    shortMarketValue :: String,
    positionMarketValue :: String,
    initialMargin :: String,
    maintenanceMargin :: String,
    lastMaintenanceMargin :: String,
    sma :: String,
    daytradeCount :: Int,
    balanceAsOf :: String,
    intradayAdjustments :: String,
    pendingRegTafFees :: String
  }
  deriving (Show, Generic, ToJSON)

instance FromJSON Account where
  parseJSON =
    genericParseJSON
      defaultOptions
        { fieldLabelModifier = \case
            "id_" -> "id"
            "adminConfigurations" -> "admin_configurations"
            "userConfigurations" -> "user_configurations"
            "accountNumber" -> "account_number"
            "buyingPower" -> "buying_power"
            "regulationBuyingPower" -> "regt_buying_power"
            "daytradingBuyingPower" -> "daytrading_buying_power"
            "effectiveBuyingPower" -> "effective_buying_power"
            "nonMarginableBuyingPower" -> "non_marginable_buying_power"
            "cryptoStatus" -> "crypto_status"
            "beginningOfDayBuyingPower" -> "bod_dtbp"
            "portfolioValue" -> "portfolio_value"
            "patternDayTrader" -> "pattern_day_trader"
            "tradingBlocked" -> "trading_blocked"
            "transfersBlocked" -> "transfers_blocked"
            "accountBlocked" -> "account_blocked"
            "accruedFees" -> "accrued_fees"
            "createdAt" -> "created_at"
            "tradeSuspendedByUser" -> "trade_suspended_by_user"
            "shortingEnabled" -> "shorting_enabled"
            "lastEquity" -> "last_equity"
            "longMarketValue" -> "long_market_value"
            "shortMarketValue" -> "short_market_value"
            "positionMarketValue" -> "position_market_value"
            "initialMargin" -> "initial_margin"
            "maintenanceMargin" -> "maintenance_margin"
            "lastMaintenanceMargin" -> "last_maintenance_margin"
            "simpleMovingAverage" -> "sma"
            "daytradeCount" -> "daytrade_count"
            "balanceAsOf" -> "balance_asof"
            "intradayAdjustments" -> "intraday_adjustments"
            "pendingRegTafFees" -> "pending_reg_taf_fees"
            x -> x
        }
