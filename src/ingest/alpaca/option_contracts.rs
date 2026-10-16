//! Alpaca's listing of active option contracts, kept as the pages served for the open interest each row carries; only
//! the number of contracts is read here.

use serde::Deserialize;
use serde::de::IgnoredAny;

use super::super::retry::{FetchError, send, with_retries};
use super::{Alpaca, paginate};
use crate::common::time::SessionDate;
use crate::ingest::Fetched;

const PAGE_LIMIT: &str = "10000";

/// How far past the session the listing reaches, ten years with leap days to spare; the endpoint's own upper bound is the
/// coming weekend, so both bounds are always sent.
const EXPIRATION_DAYS: i64 = 3_660;

#[derive(Deserialize)]
struct ContractsPage {
    /// Required, so a response without it fails rather than reading as a page of no contracts.
    option_contracts: Vec<IgnoredAny>,
}

impl Alpaca {
    /// Every active contract expiring from `session` through `EXPIRATION_DAYS` after it, as the number listed.
    pub async fn option_contracts(&self, session: SessionDate) -> Fetched<u64> {
        let first = session.to_string();
        let last = session.plus_calendar_days(EXPIRATION_DAYS).to_string();
        let (first, last) = (&first, &last);
        let pages = paginate(|page_token| async move {
            with_retries(|| {
                let mut query = vec![
                    ("status", "active"),
                    ("expiration_date_gte", first.as_str()),
                    ("expiration_date_lte", last.as_str()),
                    ("limit", PAGE_LIMIT),
                ];
                if let Some(token) = page_token.as_deref() {
                    query.push(("page_token", token));
                }
                send(
                    self.trading(reqwest::Method::GET, "/v2/options/contracts")
                        .query(&query),
                )
            })
            .await
        })
        .await;
        let pages = match pages {
            Ok(pages) => pages,
            Err(error) => return Fetched::new(Err(error), Vec::new()),
        };
        let listed = pages
            .iter()
            .map(|page| counted(page))
            .sum::<Result<u64, FetchError>>();
        Fetched::new(listed, pages)
    }
}

/// The contracts one page lists.
fn counted(body: &[u8]) -> Result<u64, FetchError> {
    let page: ContractsPage =
        serde_json::from_slice(body).map_err(|error| FetchError::Malformed {
            reason: format!("an option contracts page did not parse: {error}"),
        })?;
    Ok(u64::try_from(page.option_contracts.len())
        .expect("a page lists fewer than u64::MAX contracts"))
}

#[cfg(test)]
mod tests {
    use super::*;

    /// A page counts its contracts whatever their fields, and an empty page counts none.
    #[test]
    fn test_a_page_counts_its_contracts() {
        let page = br#"{"option_contracts":[{"symbol":"ABC261016C00010000","open_interest":"7"},{"symbol":"ABC261016P00010000","open_interest":null}],"next_page_token":"Mg=="}"#;
        assert_eq!(counted(page).unwrap(), 2);
        assert_eq!(
            counted(br#"{"option_contracts":[],"next_page_token":null}"#).unwrap(),
            0
        );
    }

    /// A body without the contracts array is refused rather than read as a page of none.
    #[test]
    fn test_a_page_without_contracts_is_refused() {
        assert!(matches!(
            counted(br#"{"message":"unauthorized."}"#),
            Err(FetchError::Malformed { .. })
        ));
    }
}
