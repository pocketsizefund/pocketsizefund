//! What a study read: one archive series over a window of sessions, each partition with the version read, so a later
//! rewrite of any of them is visible against the record.

use std::collections::BTreeMap;

use serde::{Deserialize, Serialize};

use crate::common::market::record::BarInterval;
use crate::common::storage::{EntityTag, Provider};
use crate::common::time::calendar::TradingCalendar;
use crate::common::time::{SessionDate, SessionRange, SessionRangeRefusal};

/// One archive series a study can read, under the name the journal stores.
#[derive(
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
    Hash,
    Serialize,
    Deserialize,
    strum::Display,
    strum::EnumString,
    strum::IntoStaticStr,
    strum::EnumIter,
)]
#[serde(rename_all = "snake_case")]
#[strum(serialize_all = "snake_case")]
pub enum DatasetLeg {
    /// Daily bars the vendor aggregated from trades, read from Massive's partitions.
    DailyVendorBars,
    /// Trade bars folded from ticks, Massive's where it holds the session and Alpaca's otherwise; the three cadences
    /// are three series.
    MinuteTradeBars,
    FiveMinuteTradeBars,
    DailyTradeBars,
}

impl DatasetLeg {
    /// The trade-bar leg at `interval`.
    pub fn trade_bars(interval: BarInterval) -> Self {
        match interval {
            BarInterval::OneMinute => Self::MinuteTradeBars,
            BarInterval::FiveMinute => Self::FiveMinuteTradeBars,
            BarInterval::OneDay => Self::DailyTradeBars,
        }
    }

    /// The cadence of a trade-bar leg, `None` for a leg of vendor bars.
    pub fn trades_interval(self) -> Option<BarInterval> {
        match self {
            Self::DailyVendorBars => None,
            Self::MinuteTradeBars => Some(BarInterval::OneMinute),
            Self::FiveMinuteTradeBars => Some(BarInterval::FiveMinute),
            Self::DailyTradeBars => Some(BarInterval::OneDay),
        }
    }
}

/// One session's partition as read: the provider whose partition it was and the entity tag of the version read.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct PartitionRead {
    provider: Provider,
    tag: EntityTag,
}

impl PartitionRead {
    pub fn new(provider: Provider, tag: EntityTag) -> Self {
        Self { provider, tag }
    }

    pub fn provider(&self) -> Provider {
        self.provider
    }

    pub fn tag(&self) -> &EntityTag {
        &self.tag
    }
}

/// The partitions a study read, by session with the provider and version read, and every trading session in the
/// window that had none.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
#[serde(try_from = "FingerprintFields")]
pub struct Fingerprint {
    leg: DatasetLeg,
    #[serde(flatten)]
    range: SessionRange,
    partitions: BTreeMap<SessionDate, PartitionRead>,
    /// Trading sessions in the window with no partition, so an absence is read as one rather than as a short window.
    missing: Vec<SessionDate>,
}

#[derive(Deserialize)]
struct FingerprintFields {
    leg: DatasetLeg,
    first: SessionDate,
    last: SessionDate,
    partitions: BTreeMap<SessionDate, PartitionRead>,
    missing: Vec<SessionDate>,
}

impl TryFrom<FingerprintFields> for Fingerprint {
    type Error = FingerprintRefusal;

    /// Checks what holds without a calendar; that every session trades was checked when the fingerprint was taken.
    fn try_from(fields: FingerprintFields) -> Result<Self, Self::Error> {
        let range =
            SessionRange::new(fields.first, fields.last).map_err(FingerprintRefusal::Inverted)?;
        let outside = |session: &&SessionDate| !range.contains(**session);
        if let Some(session) = fields
            .partitions
            .keys()
            .chain(&fields.missing)
            .find(outside)
        {
            return Err(FingerprintRefusal::NotATradingSession { session: *session });
        }
        if let Some(session) = fields
            .missing
            .iter()
            .find(|session| fields.partitions.contains_key(session))
        {
            return Err(FingerprintRefusal::ReadAndMissing { session: *session });
        }
        if !fields.missing.is_sorted() {
            return Err(FingerprintRefusal::MissingUnordered);
        }
        Ok(Self {
            leg: fields.leg,
            range,
            partitions: fields.partitions,
            missing: fields.missing,
        })
    }
}

/// Why a fingerprint could not be taken.
#[derive(Debug, Clone, PartialEq, Eq, thiserror::Error)]
pub enum FingerprintRefusal {
    #[error("{0}")]
    Inverted(SessionRangeRefusal),
    #[error("the calendar does not cover {} to {}", .range.first(), .range.last())]
    CalendarShort { range: SessionRange },
    /// A partition for a day the calendar does not trade, or outside the window.
    #[error("a partition for {session}, which is not a trading session in the window")]
    NotATradingSession { session: SessionDate },
    #[error("{session} is both read and missing")]
    ReadAndMissing { session: SessionDate },
    #[error("the missing sessions are out of order")]
    MissingUnordered,
}

impl Fingerprint {
    /// `partitions` holds what was found; every other trading session in `range` is recorded missing. Taken only by the
    /// crate's loaders, so a study cannot vouch for partitions nothing read.
    pub(crate) fn new(
        leg: DatasetLeg,
        range: SessionRange,
        calendar: &TradingCalendar,
        partitions: BTreeMap<SessionDate, PartitionRead>,
    ) -> Result<Self, FingerprintRefusal> {
        if !calendar.covers(range) {
            return Err(FingerprintRefusal::CalendarShort { range });
        }
        let sessions = calendar.trading_days_in_range(range);
        if let Some(session) = partitions
            .keys()
            .find(|session| !sessions.contains(session))
        {
            return Err(FingerprintRefusal::NotATradingSession { session: *session });
        }
        let missing = sessions
            .into_iter()
            .filter(|session| !partitions.contains_key(session))
            .collect();
        Ok(Self {
            leg,
            range,
            partitions,
            missing,
        })
    }

    pub fn leg(&self) -> DatasetLeg {
        self.leg
    }

    pub fn partitions(&self) -> &BTreeMap<SessionDate, PartitionRead> {
        &self.partitions
    }

    pub fn missing(&self) -> &[SessionDate] {
        &self.missing
    }

    /// Every partition read whose provider or version is no longer the one read; `current` holds what each session
    /// would be read as now, and a session absent from it is gone. A study reading one of these rests on data that has
    /// since been rewritten or replaced.
    pub fn contaminated(
        &self,
        current: &BTreeMap<SessionDate, PartitionRead>,
    ) -> Vec<Contamination> {
        self.partitions
            .iter()
            .filter(|(session, read)| current.get(session) != Some(read))
            .map(|(session, read)| Contamination {
                session: *session,
                read: read.clone(),
                now: current.get(session).cloned(),
            })
            .collect()
    }
}

/// A session read from one provider and version that now reads as another, or is gone.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Contamination {
    session: SessionDate,
    read: PartitionRead,
    now: Option<PartitionRead>,
}

impl Contamination {
    pub fn session(&self) -> SessionDate {
        self.session
    }

    /// The provider and version the session was read from.
    pub fn read(&self) -> &PartitionRead {
        &self.read
    }

    /// The provider and version the session reads as now, `None` when it is gone.
    pub fn now(&self) -> Option<&PartitionRead> {
        self.now.as_ref()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    use chrono::{NaiveDate, NaiveTime};

    use crate::common::time::calendar::TradingSession;

    fn session(day: u32) -> SessionDate {
        SessionDate::from_date(NaiveDate::from_ymd_opt(2026, 9, day).unwrap())
    }

    /// 2026-09-21 to 2026-09-27: Monday to Friday trade.
    fn calendar() -> TradingCalendar {
        let open = NaiveTime::from_hms_opt(9, 30, 0).unwrap();
        let close = NaiveTime::from_hms_opt(16, 0, 0).unwrap();
        TradingCalendar::new(
            (21..=25)
                .map(|day| TradingSession::new(session(day), open, close).unwrap())
                .collect(),
            range(21, 27),
        )
        .unwrap()
    }

    fn range(first: u32, last: u32) -> SessionRange {
        SessionRange::new(session(first), session(last)).unwrap()
    }

    fn read(provider: Provider, tag: &str) -> PartitionRead {
        PartitionRead::new(provider, EntityTag::new(tag))
    }

    fn tags(days: &[u32]) -> BTreeMap<SessionDate, PartitionRead> {
        days.iter()
            .map(|day| {
                (
                    session(*day),
                    read(Provider::Massive, &format!("\"tag-{day}\"")),
                )
            })
            .collect()
    }

    #[test]
    fn test_every_trading_session_is_read_or_named_missing() {
        let fingerprint = Fingerprint::new(
            DatasetLeg::DailyVendorBars,
            range(21, 27),
            &calendar(),
            tags(&[21, 22, 25]),
        )
        .unwrap();
        assert_eq!(
            fingerprint.partitions().keys().copied().collect::<Vec<_>>(),
            [session(21), session(22), session(25)]
        );
        assert_eq!(fingerprint.missing(), [session(23), session(24)]);
        assert_eq!(fingerprint.leg(), DatasetLeg::DailyVendorBars);
    }

    #[test]
    fn test_a_fingerprint_refuses_what_it_could_not_have_read() {
        assert_eq!(
            Fingerprint::new(
                DatasetLeg::DailyVendorBars,
                range(21, 28),
                &calendar(),
                tags(&[])
            ),
            Err(FingerprintRefusal::CalendarShort {
                range: range(21, 28)
            })
        );
        // Saturday, and a session outside the window.
        for day in [26, 25] {
            assert_eq!(
                Fingerprint::new(
                    DatasetLeg::DailyVendorBars,
                    range(21, 24),
                    &calendar(),
                    tags(&[21, day])
                ),
                Err(FingerprintRefusal::NotATradingSession {
                    session: session(day)
                })
            );
        }
    }

    #[test]
    fn test_a_partition_rewritten_or_gone_since_it_was_read_is_contaminated() {
        let fingerprint = Fingerprint::new(
            DatasetLeg::DailyVendorBars,
            range(21, 25),
            &calendar(),
            tags(&[21, 22, 24]),
        )
        .unwrap();
        let mut current = tags(&[21, 22, 24, 25]);
        assert_eq!(fingerprint.contaminated(&current), []);
        // The same tag under another provider is another partition.
        current.insert(session(21), read(Provider::Alpaca, "\"tag-21\""));
        current.insert(session(22), read(Provider::Massive, "\"rewritten\""));
        current.remove(&session(24));
        assert_eq!(
            fingerprint.contaminated(&current),
            [
                Contamination {
                    session: session(21),
                    read: read(Provider::Massive, "\"tag-21\""),
                    now: Some(read(Provider::Alpaca, "\"tag-21\"")),
                },
                Contamination {
                    session: session(22),
                    read: read(Provider::Massive, "\"tag-22\""),
                    now: Some(read(Provider::Massive, "\"rewritten\"")),
                },
                Contamination {
                    session: session(24),
                    read: read(Provider::Massive, "\"tag-24\""),
                    now: None,
                },
            ]
        );
    }

    #[test]
    fn test_a_stored_fingerprint_must_agree_with_its_own_window() {
        let stored = serde_json::to_value(
            Fingerprint::new(
                DatasetLeg::DailyVendorBars,
                range(21, 25),
                &calendar(),
                tags(&[21, 24]),
            )
            .unwrap(),
        )
        .unwrap();
        let mut outside = stored.clone();
        outside["partitions"]["2026-09-28"] = serde_json::json!("\"tag\"");
        let mut both = stored.clone();
        both["missing"] = serde_json::json!(["2026-09-21", "2026-09-22"]);
        let mut unordered = stored.clone();
        unordered["missing"] = serde_json::json!(["2026-09-23", "2026-09-22"]);
        let mut inverted = stored;
        inverted["last"] = serde_json::json!("2026-09-20");
        for refused in [outside, both, unordered] {
            assert!(
                serde_json::from_value::<Fingerprint>(refused.clone()).is_err(),
                "{refused}"
            );
        }
        assert_eq!(
            serde_json::from_value::<Fingerprint>(inverted)
                .unwrap_err()
                .to_string(),
            "the range 2026-09-21 to 2026-09-20 ends before it starts"
        );
    }

    #[test]
    fn test_a_fingerprint_reads_back_as_written() {
        let fingerprint = Fingerprint::new(
            DatasetLeg::DailyVendorBars,
            range(21, 25),
            &calendar(),
            tags(&[21, 24]),
        )
        .unwrap();
        let encoded = serde_json::to_string(&fingerprint).unwrap();
        assert_eq!(
            encoded,
            r#"{"leg":"daily_vendor_bars","first":"2026-09-21","last":"2026-09-25","partitions":{"2026-09-21":{"provider":"massive","tag":"\"tag-21\""},"2026-09-24":{"provider":"massive","tag":"\"tag-24\""}},"missing":["2026-09-22","2026-09-23","2026-09-25"]}"#
        );
        assert_eq!(
            serde_json::from_str::<Fingerprint>(&encoded).unwrap(),
            fingerprint
        );
    }

    /// Each leg keeps the name journals already hold, and serde and strum agree on it.
    #[test]
    fn test_a_dataset_leg_round_trips_under_its_stored_name() {
        use strum::IntoEnumIterator;
        assert_eq!(
            DatasetLeg::iter().map(<&str>::from).collect::<Vec<_>>(),
            [
                "daily_vendor_bars",
                "minute_trade_bars",
                "five_minute_trade_bars",
                "daily_trade_bars"
            ]
        );
        for leg in DatasetLeg::iter() {
            let stored = serde_json::to_value(leg).unwrap();
            assert_eq!(stored, serde_json::json!(leg.to_string()));
            assert_eq!(serde_json::from_value::<DatasetLeg>(stored).unwrap(), leg);
            assert_eq!(leg.to_string().parse::<DatasetLeg>().unwrap(), leg);
        }
    }

    /// Every interval names one trade-bar leg and that leg reads back the interval; the vendor leg has none.
    #[test]
    fn test_a_trade_bar_leg_round_trips_through_its_interval() {
        use strum::IntoEnumIterator;
        let legs: Vec<(BarInterval, &str)> = BarInterval::iter()
            .map(|interval| (interval, DatasetLeg::trade_bars(interval).into()))
            .collect();
        assert_eq!(
            legs,
            [
                (BarInterval::OneMinute, "minute_trade_bars"),
                (BarInterval::FiveMinute, "five_minute_trade_bars"),
                (BarInterval::OneDay, "daily_trade_bars"),
            ]
        );
        for interval in BarInterval::iter() {
            assert_eq!(
                DatasetLeg::trade_bars(interval).trades_interval(),
                Some(interval)
            );
        }
        assert_eq!(DatasetLeg::DailyVendorBars.trades_interval(), None);
    }
}
