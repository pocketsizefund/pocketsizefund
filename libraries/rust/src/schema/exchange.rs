use super::asset_class::AssetClass;
use super::locale::Locale;
use anyhow::{Error, Result};
use serde::{Deserialize, Serialize};

#[derive(Debug, Serialize, Deserialize, PartialEq)]
pub struct Exchange {
    acronym: String,
    asset_class: AssetClass,
    locale: Locale,
}

#[cfg(test)]
mod tests {
    use super::*;
    use serde_json::{from_value, json};

    #[tok::test]
    fn test_us_local() {
        let input: Exchange = from_value(json!({
                "acronym": "AMEX",
                "asset_class": "stocks",
                "locale": "us"
        }))
        .unwrap();

        assert_eq!(
            input,
            Exchange {
                acronym: "AMEX".to_string(),
                asset_class: AssetClass::Stocks,
                locale: Locale::US
            }
        );
    }
}
