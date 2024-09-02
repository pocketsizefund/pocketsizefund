use apca::api::v2::account::Account as AlpacaAccount;
use chrono::{DateTime, Utc};
use rusty_money::{iso, FormattableCurrency, Money};
use serde::{Deserialize, Serialize};

#[derive(Debug, Serialize, Deserialize)]
pub enum AccountStatus {
    /// The account is onboarding.
    #[serde(rename = "ONBOARDING")]
    Onboarding,
    /// The account application submission failed for some reason.
    #[serde(rename = "SUBMISSION_FAILED")]
    SubmissionFailed,
    /// The account application has been submitted for review.
    #[serde(rename = "SUBMITTED")]
    Submitted,
    /// The account information is being updated.
    #[serde(rename = "ACCOUNT_UPDATED")]
    AccountUpdated,
    /// The final account approval is pending.
    #[serde(rename = "APPROVAL_PENDING")]
    ApprovalPending,
    /// The account is active for trading.
    #[serde(rename = "ACTIVE")]
    Active,
    /// The account application has been rejected.
    #[serde(rename = "REJECTED")]
    Rejected,
}

#[derive(Debug, Serialize, Deserialize)]
pub struct Alpaca {
    /// Alpaca Account ID
    id: String,
    /// Alpaca Account number
    account_number: String,
    /// See account statuses Enum.
    status: AccountStatus,
    /// THe curent status of the crypto enablement. See detailed crypto statuses Enum.
    crypto_status: AccountStatus,
    /// The currency of the account.
    currency: String,
    /// Cash balance
    cash: Money,
    /// Current available non-margin dollar buying power
    non_marginable_buying_power: Money,
    /// The fees collected.
    accrued_fees: Money,
    /// Cash pending transfer in.
    pending_transfer_in: Money,
    /// Cash pending transfer out
    pending_transfer_out: Money,
    ///  Whether or not the account has been flagged as a pattern day trader
    pattern_day_trader: bool,
    /// User setting. If true, the account is not allowed to place orders.
    trade_suspended_by_user: bool,
    /// If true, the account is not allowed to place orders.
    trading_blocked: bool,
    /// If true, the account is not allowed to request money transfers.
    transfers_blocked: bool,
    /// If true, the account activity by user is prohibited.
    account_blocked: bool,
    /// Timestamp this account was created at
    created_at: DateTime<Utc>,
    /// Flag to denote whether or not the account is permitted to short
    shorting_enabled: bool,
    /// Real-time MtM value of all long positions held in the account
    long_market_value: Money,
    /// Real-time MtM value of all short positions held in the account
    short_market_value: Money,
    /// cash + long_market_value + short_market_value
    equity: Money,
    /// Equity as of previous trading day at 16:00:00 ET
    last_equity: Money,
    /// Buying power (BP) multiplier that represents account margin classification
    // Valid values:
    // - 1 (standard limited margin account with 1x BP)
    // - 2 (reg T margin account with 2x intraday and overnight BP; this is the default for all non-PDT accounts with $2,000 or more equity)
    // - 4 (PDT account with 4x intraday BP and 2x reg T overnight BP)
    multiplier: u32,
    /// Current available $ buying power;
    /// If multiplier = 4, this is your daytrade buying power which is calculated as (lastequity - (last) maintenance_margin) 4;
    /// If multiplier = 2, buyingpower = max(equity – initial_margin,0) 2;
    /// If multiplier = 1, buying_power = cash
    buying_power: f32,
    // Reg T initial margin requirement (continuously updated value)
    initial_margin: Money<'a, C>,
    /// Maintenance margin requirement (continuously updated value)
    maintenance_margin: Money<'a, C>,
    /// Value of special memorandum account (will be used at a later date to provide additional buying_power)
    sma: f32,
    /// The current number of daytrades that have been made in the last 5 trading days (inclusive of today)
    daytrade_count: u32,
    /// Your maintenance margin requirement on the previous trading day
    last_maintenance_margin: Money<'a, C>,
    /// Your buying power for day trades (continuously updated value)
    daytrading_buying_power: f32,
    /// Your buying power under Regulation T (your excess equity - equity minus margin value - times your margin multiplier)
    regt_buying_power: f32,
}

#[cfg(test)]
mod tests {
    use super::*;
    use serde_json::json;
    use test_log::test;

    #[test]
    fn test_get_account() {
        let api_response = json!({
          "account_blocked": false,
          "account_number": "010203ABCD",
          "buying_power": "262113.632",
          "cash": "-23140.2",
          "created_at": "2019-06-12T22:47:07.99658Z",
          "currency": "USD",
          "crypto_status": "ACTIVE",
          "non_marginable_buying_power": "7386.56",
          "accrued_fees": "0",
          "pending_transfer_in": "0",
          "pending_transfer_out": "0",
          "daytrade_count": "0",
          "daytrading_buying_power": "262113.632",
          "equity": "103820.56",
          "id": "e6fe16f3-64a4-4921-8928-cadf02f92f98",
          "initial_margin": "63480.38",
          "last_equity": "103529.24",
          "last_maintenance_margin": "38000.832",
          "long_market_value": "126960.76",
          "maintenance_margin": "38088.228",
          "multiplier": "4",
          "pattern_day_trader": false,
          "portfolio_value": "103820.56",
          "regt_buying_power": "80680.36",
          "short_market_value": "0",
          "shorting_enabled": true,
          "sma": "0",
          "status": "ACTIVE",
          "trade_suspended_by_user": false,
          "trading_blocked": false,
          "transfers_blocked": false
        });

        let account =
            serde_json::from_str::<AlpacaAccount>(&serde_json::to_string(&api_response).unwrap())
                .unwrap();

        assert_eq!(account.account_blocked, false);
    }
}
