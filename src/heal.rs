//! The nightly heal: each leg's owed sessions fetched and written to the archive, oldest first, until done or out of
//! time, with each partition journaled once it has been read back.

use std::collections::BTreeMap;
use std::future::Future;
use std::num::{NonZeroU16, NonZeroU64, NonZeroUsize};
use std::path::Path;
use std::sync::Arc;
use std::time::Duration;

use chrono::{DateTime, Utc};
use strum::IntoEnumIterator;
use tokio::sync::mpsc::{self, Sender};
use tokio::task::JoinSet;
use tokio::time::Instant;

use crate::archive::bars::{Provenance, Subscription};
use crate::archive::raw::TeeError;
use crate::archive::reference::{
    conditions_key, decode_series_boundaries, encode_conditions, encode_security_details,
    encode_series_boundaries, encode_splits, latest_conditions, latest_snapshot,
};
use crate::archive::{self, Archive, ArchiveError};
use crate::archive::{bars, quote_bars, trade_bars};
use crate::common::heal::{
    Coverage, CoverageRefusal, Held, KeepsRefusal, Leg, PartitionFailureKind, SeriesListing,
    SessionOutcome, WindowRefusal, alpaca_five_minute_bars, alpaca_minute_bars, alpaca_quotes,
    alpaca_series_boundaries, alpaca_trades, fetched_range, massive_daily_bars,
    massive_security_details, massive_splits, owed, window,
};
use crate::common::journal::{
    ArchiveSurveyed, ConditionsWritten, ConfigurationResolved, HealFinished, Observation,
    PartitionFailed, PartitionWritten, Unanswered, quotes_folded, trades_folded,
};
use crate::common::market::Symbol;
use crate::common::market::aggregate::{self, RollsUp, session_bars};
use crate::common::market::corporate_actions::refresh_boundaries;
use crate::common::market::quote_bars::{QuoteFold, QuoteFoldRefusal};
use crate::common::market::record::{Bar, BarInterval, Partition, PartitionRefusal};
use crate::common::market::security_details::SecurityDetails;
use crate::common::market::trade_bars::{TradeConditions, TradeFold};
use crate::common::monoid::{Monoid, concatenate};
use crate::common::parameter::{Parameter, ParameterRefusal, at_most, record};
use crate::common::storage::{Key, Provider, ReferenceKey, ReferenceTable, SeriesPrefix};
use crate::common::time::calendar::TradingCalendar;
use crate::common::time::{SessionDate, SessionRange};
use crate::ingest::alpaca::corporate_actions::SeriesBoundaries;
use crate::ingest::alpaca::{
    Alpaca, AlpacaQuoteOutcome, AlpacaTradeOutcome, MinuteBars, Page, invalid_symbol,
};
use crate::ingest::massive::{DetailsAnswer, Massive};
use crate::ingest::{FetchError, Fetched, RefusedRow, refused_by_cause};
use crate::journal::Journal;
use crate::parameter::{Directories, environment_variable};

const DEFAULT_LOOKBACK_SESSIONS: NonZeroUsize = NonZeroUsize::new(5).expect("5 is not zero");
const DEFAULT_BUDGET_MINUTES: NonZeroU64 = NonZeroU64::new(240).expect("240 is not zero");
/// A whole-market session measured 2026-09-30 at about 33 s with these two.
const DEFAULT_MINUTE_BATCH_SYMBOLS: NonZeroUsize = NonZeroUsize::new(200).expect("200 is not zero");
const DEFAULT_MINUTE_CONCURRENCY: NonZeroUsize = NonZeroUsize::new(8).expect("8 is not zero");
/// Each symbol's ticks are one serial chain of pages, so a session is bounded by its longest names.
const DEFAULT_TICK_CONCURRENCY: NonZeroUsize = NonZeroUsize::new(16).expect("16 is not zero");
/// Five years of sessions, as far back as Massive Starter reaches.
const MAXIMUM_LOOKBACK_SESSIONS: NonZeroUsize =
    NonZeroUsize::new(1_260).expect("1,260 is not zero");
/// One day, past which one night's run would meet the next.
const MAXIMUM_BUDGET_MINUTES: NonZeroU64 = NonZeroU64::new(1_440).expect("1,440 is not zero");

/// The heal's settings, each resolved once at startup.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Parameters {
    lookback_sessions: NonZeroUsize,
    budget: Duration,
    directories: Directories,
    minute_batch_symbols: NonZeroUsize,
    minute_concurrency: NonZeroUsize,
    tick_concurrency: NonZeroUsize,
}

impl Parameters {
    /// Reads each parameter's variable, returning the configuration the journal records for them.
    pub fn from_environment() -> Result<(Self, ConfigurationResolved), ParameterRefusal> {
        Self::resolved(&environment_variable)
    }

    /// Resolves each parameter from what `supplied` returns for it, or its default when that is nothing.
    fn resolved(
        supplied: &impl Fn(Parameter) -> Result<Option<String>, ParameterRefusal>,
    ) -> Result<(Self, ConfigurationResolved), ParameterRefusal> {
        let mut resolved = BTreeMap::new();
        let read = |parameter| Ok::<_, ParameterRefusal>((parameter, supplied(parameter)?));
        let budget_minutes = at_most(
            Parameter::BudgetMinutes,
            record(
                read(Parameter::BudgetMinutes)?,
                DEFAULT_BUDGET_MINUTES,
                &mut resolved,
            )?,
            MAXIMUM_BUDGET_MINUTES,
        )?;
        let parameters = Self {
            lookback_sessions: at_most(
                Parameter::LookbackSessions,
                record(
                    read(Parameter::LookbackSessions)?,
                    DEFAULT_LOOKBACK_SESSIONS,
                    &mut resolved,
                )?,
                MAXIMUM_LOOKBACK_SESSIONS,
            )?,
            budget: Duration::from_secs(budget_minutes.get() * 60),
            directories: Directories::resolved(supplied, &mut resolved)?,
            minute_batch_symbols: record(
                read(Parameter::MinuteBatchSymbols)?,
                DEFAULT_MINUTE_BATCH_SYMBOLS,
                &mut resolved,
            )?,
            minute_concurrency: record(
                read(Parameter::MinuteConcurrency)?,
                DEFAULT_MINUTE_CONCURRENCY,
                &mut resolved,
            )?,
            tick_concurrency: record(
                read(Parameter::TickConcurrency)?,
                DEFAULT_TICK_CONCURRENCY,
                &mut resolved,
            )?,
        };
        Ok((parameters, ConfigurationResolved::new(resolved)))
    }

    pub fn journal_directory(&self) -> &Path {
        self.directories.journal()
    }

    pub fn log_directory(&self) -> &Path {
        self.directories.log()
    }
}

/// Why the heal could not decide what it owes, so wrote nothing.
#[derive(Debug, thiserror::Error)]
pub enum HealError {
    #[error("fetching the calendar failed: {0}")]
    Calendar(FetchError),
    #[error("{0}")]
    Window(WindowRefusal),
    /// A leg's sessions could not be read off the calendar, which the run fetched to reach back far enough.
    #[error("the {leg} leg: {refusal}")]
    Keeps { leg: Leg, refusal: KeepsRefusal },
    #[error("{0}")]
    List(ArchiveError),
    /// A series spans sessions the fetched calendar does not reach.
    #[error("the {series} series: {refusal}")]
    Coverage {
        series: String,
        refusal: CoverageRefusal,
    },
    /// A write or failure went unrecorded, so the run stops rather than go on with what it cannot record.
    #[error("journaling the heal failed: {0}")]
    Journal(std::io::Error),
}

/// Why one owed partition was not written.
#[derive(Debug, thiserror::Error)]
pub enum PartitionFailure {
    #[error("{0}")]
    Fetch(FetchError),
    /// The vendor answered a trading day with nothing, which written would mark the session held for good.
    #[error("the vendor answered with no rows")]
    NoRows,
    #[error("{session} is not in the calendar")]
    NotInCalendar { session: SessionDate },
    #[error("{0}")]
    Encode(archive::EncodeRefusal),
    #[error("{0}")]
    Decode(archive::DecodeRefusal),
    #[error("{0}")]
    Archive(ArchiveError),
    /// Listed, then gone when read.
    #[error("{} vanished", .key.path())]
    Vanished { key: Key },
    /// The session's daily bars, which name its symbols, are not written.
    #[error("no daily bars at {} to take the symbols from", .key.path())]
    NoSymbols { key: Key },
    /// The session's minute bars, which roll up into its five-minute bars, are not written.
    #[error("no minute bars at {} to roll up", .key.path())]
    NoMinutes { key: Key },
    #[error("{0}")]
    Fold(QuoteFoldRefusal),
    #[error("{0}")]
    Tee(TeeError),
    /// A write the partition needed could not be journaled, which stops the run.
    #[error("journaling a write failed: {0}")]
    Journal(std::io::Error),
}

impl PartitionFailure {
    pub fn kind(&self) -> PartitionFailureKind {
        match self {
            Self::Fetch(_) => PartitionFailureKind::Fetch,
            Self::NoRows => PartitionFailureKind::NoRows,
            Self::NotInCalendar { .. } => PartitionFailureKind::NotInCalendar,
            Self::Encode(_) => PartitionFailureKind::Encode,
            Self::Decode(_) => PartitionFailureKind::Decode,
            Self::Archive(_) => PartitionFailureKind::Archive,
            Self::Vanished { .. } => PartitionFailureKind::Vanished,
            Self::NoSymbols { .. } => PartitionFailureKind::NoSymbols,
            Self::NoMinutes { .. } => PartitionFailureKind::NoMinutes,
            Self::Fold(_) => PartitionFailureKind::Fold,
            Self::Tee(_) => PartitionFailureKind::Tee,
            Self::Journal(_) => PartitionFailureKind::Journal,
        }
    }

    /// The record of this failure, journaled as it happens.
    fn failed(&self, leg: Leg, session: SessionDate) -> PartitionFailed {
        PartitionFailed::new(leg, session, self.kind(), self.to_string())
    }

    /// The outcome the journal records, its kind beside its text.
    fn outcome(&self) -> SessionOutcome {
        SessionOutcome::Failed {
            failure: self.kind(),
            cause: self.to_string(),
        }
    }
}

impl From<FetchError> for PartitionFailure {
    fn from(error: FetchError) -> Self {
        Self::Fetch(error)
    }
}

impl From<ArchiveError> for PartitionFailure {
    fn from(error: ArchiveError) -> Self {
        Self::Archive(error)
    }
}

impl From<archive::EncodeRefusal> for PartitionFailure {
    fn from(refusal: archive::EncodeRefusal) -> Self {
        Self::Encode(refusal)
    }
}

impl From<archive::DecodeRefusal> for PartitionFailure {
    fn from(refusal: archive::DecodeRefusal) -> Self {
        Self::Decode(refusal)
    }
}

/// The clients the heal reads from and writes to.
pub struct Clients {
    archive: Archive,
    /// Shared by the concurrent security details requests.
    massive: Arc<Massive>,
    /// Shared by the concurrent one-minute batches, so its secret is held once rather than copied into each.
    alpaca: Arc<Alpaca>,
}

impl Clients {
    pub fn new(archive: Archive, massive: Massive, alpaca: Alpaca) -> Self {
        Self {
            archive,
            massive: Arc::new(massive),
            alpaca: Arc::new(alpaca),
        }
    }

    /// Writes one object to the archive, as `publish` and `write_conditions` take it.
    fn put(&self) -> impl AsyncFn(&Key, Vec<u8>) -> Result<(), ArchiveError> {
        async |key: &Key, body| self.archive.put(key, body).await
    }

    /// Stores a snapshot's response bodies as served under its raw key, deleting the parsed snapshot first so one that
    /// fails before it is published again reads as owed.
    fn store(
        &self,
    ) -> impl AsyncFn(ReferenceKey, &Provenance, Vec<Vec<u8>>) -> Result<(), PartitionFailure> {
        async |key: ReferenceKey, provenance: &Provenance, bodies: Vec<Vec<u8>>| {
            self.archive.delete(&Key::from(key)).await?;
            let mut writer = self
                .archive
                .begin_raw(Key::RawReference(key), provenance)
                .await
                .map_err(PartitionFailure::Tee)?;
            for body in &bodies {
                if let Err(error) = writer.push_line(body).await {
                    writer.abort().await;
                    return Err(PartitionFailure::Tee(error));
                }
            }
            writer.finish().await.map_err(PartitionFailure::Tee)?;
            Ok(())
        }
    }
}

/// Heals the window of trading days before `today`. Every owed session of every leg ends with an outcome; the budget
/// is checked before each session starts, so a session under way always finishes.
pub async fn run(
    parameters: &Parameters,
    clients: &Clients,
    journal: &mut Journal,
    today: SessionDate,
) -> Result<HealFinished, HealError> {
    let deadline = Instant::now() + parameters.budget;
    let calendar = clients
        .alpaca
        .calendar(fetched_range(today, parameters.lookback_sessions))
        .await
        .map_err(HealError::Calendar)?;
    let window =
        window(&calendar, today, parameters.lookback_sessions).map_err(HealError::Window)?;
    let mut outcomes = BTreeMap::new();
    let mut unrecognized = BTreeMap::new();
    for leg in Leg::iter() {
        let mut paths = Vec::new();
        for key in leg.keys(today) {
            paths.extend(
                clients
                    .archive
                    .list(&key.series())
                    .await
                    .map_err(HealError::List)?,
            );
        }
        let held = Held::of(leg, paths);
        if !held.unrecognized().is_empty() {
            tracing::warn!(%leg, unrecognized = ?held.unrecognized(), "Objects outside the series were not counted as held");
            unrecognized.insert(leg, held.unrecognized().clone());
        }
        let kept = leg
            .keeps(&window, &calendar)
            .map_err(|refusal| HealError::Keeps { leg, refusal })?;
        let sessions = heal_leg(
            leg,
            owed(&kept, held.sessions()),
            deadline,
            journal,
            async |session, journal: &mut Journal| {
                write(leg, session, &calendar, parameters, clients, journal).await
            },
        )
        .await?;
        outcomes.insert(leg, sessions);
    }
    Ok(HealFinished::new(window, outcomes, unrecognized))
}

/// Measures every dated series of market data, raw and parsed, against the trading calendar from its first session.
/// Reference snapshots are left out, since a snapshot leg keeps only some sessions by design.
pub async fn survey(clients: &Clients, today: SessionDate) -> Result<ArchiveSurveyed, HealError> {
    let paths = clients
        .archive
        .list(&SeriesPrefix::market_data())
        .await
        .map_err(HealError::List)?;
    let listing = SeriesListing::of(paths.iter().map(String::as_str));
    let Some(reach) = listing.reach(today) else {
        return Ok(ArchiveSurveyed::new(
            BTreeMap::new(),
            listing.unrecognized(),
        ));
    };
    let calendar = clients
        .alpaca
        .calendar(reach)
        .await
        .map_err(HealError::Calendar)?;
    let coverage = listing
        .series()
        .iter()
        .map(|(series, held)| match Coverage::of(held, &calendar) {
            Ok(coverage) => Ok((series.clone(), coverage)),
            Err(refusal) => Err(HealError::Coverage {
                series: series.clone(),
                refusal,
            }),
        })
        .collect::<Result<_, _>>()?;
    Ok(ArchiveSurveyed::new(coverage, listing.unrecognized()))
}

/// Writes each of `leg`'s owed sessions with `write` until `deadline`, journaling each outcome as it lands; a journal
/// error, from the append or from inside `write`, stops the run before another session starts.
async fn heal_leg(
    leg: Leg,
    owed: impl IntoIterator<Item = SessionDate>,
    deadline: Instant,
    journal: &mut Journal,
    mut write: impl AsyncFnMut(SessionDate, &mut Journal) -> Result<PartitionWritten, PartitionFailure>,
) -> Result<BTreeMap<SessionDate, SessionOutcome>, HealError> {
    let mut sessions = BTreeMap::new();
    for session in owed {
        let outcome = if Instant::now() >= deadline {
            SessionOutcome::Unreached
        } else {
            match write(session, journal).await {
                Ok(written) => {
                    tracing::info!(%leg, %session, rows = written.rows(), refused = %written.refused(), unanswered = written.unanswered().len(), "Partition written");
                    journal
                        .append(Utc::now(), Observation::PartitionWritten(written))
                        .map_err(HealError::Journal)?;
                    SessionOutcome::Written
                }
                Err(PartitionFailure::Journal(error)) => return Err(HealError::Journal(error)),
                Err(failure) => {
                    tracing::warn!(%leg, %session, %failure, "Partition not written");
                    journal
                        .append(
                            Utc::now(),
                            Observation::PartitionFailed(failure.failed(leg, session)),
                        )
                        .map_err(HealError::Journal)?;
                    failure.outcome()
                }
            }
        };
        sessions.insert(session, outcome);
    }
    Ok(sessions)
}

/// Fetches one leg's session and writes it, returning its record, or why it was not written.
async fn write(
    leg: Leg,
    session: SessionDate,
    calendar: &TradingCalendar,
    parameters: &Parameters,
    clients: &Clients,
    journal: &mut Journal,
) -> Result<PartitionWritten, PartitionFailure> {
    match leg {
        Leg::MassiveDailyBars => write_daily_bars(session, clients, journal).await,
        Leg::AlpacaMinuteBars => write_minute_bars(session, parameters, clients, journal).await,
        Leg::AlpacaFiveMinuteBars => write_five_minute_bars(session, clients).await,
        Leg::AlpacaQuotes => {
            let hours = calendar
                .session(session)
                .map(|trading| trading.hours())
                .ok_or(PartitionFailure::NotInCalendar { session })?;
            write_quotes(session, hours, parameters, clients, journal).await
        }
        Leg::AlpacaTrades => write_trades(session, parameters, clients, journal).await,
        Leg::MassiveSplits => write_splits(session, clients, journal).await,
        Leg::AlpacaSeriesBoundaries => write_series_boundaries(session, clients, journal).await,
        Leg::MassiveSecurityDetails => {
            write_security_details(session, parameters, clients, journal).await
        }
    }
}

/// The provenance of rows `subscription` answered just now, written by this run.
fn fetched_now(subscription: Subscription, journal: &Journal) -> Provenance {
    Provenance::new(
        subscription,
        Utc::now(),
        journal.run_id(),
        journal.commit().cloned(),
    )
}

/// Encodes `rows` under `key` and writes them with `put`, returning how many were written. An empty answer is refused,
/// since for a trading day it is a vendor gap that written would mark the session held for good.
async fn publish<K: Into<Key>, Row, Refusal: Into<archive::EncodeRefusal>>(
    put: impl AsyncFnOnce(&Key, Vec<u8>) -> Result<(), ArchiveError>,
    key: K,
    rows: &[Row],
    provenance: &Provenance,
    encode: impl FnOnce(&K, &[Row], &Provenance) -> Result<Vec<u8>, Refusal>,
) -> Result<u64, PartitionFailure> {
    if rows.is_empty() {
        return Err(PartitionFailure::NoRows);
    }
    let body = encode(&key, rows, provenance).map_err(Into::into)?;
    put(&key.into(), body).await?;
    Ok(u64::try_from(rows.len()).expect("a partition holds fewer than u64::MAX rows"))
}

/// `rows` as a partition, an empty answer refused as `publish` refuses one.
fn partition<Row>(rows: Vec<Row>) -> Result<Partition<Row>, PartitionFailure> {
    Partition::try_from(rows).map_err(|refusal| match refusal {
        PartitionRefusal::Empty => PartitionFailure::NoRows,
    })
}

/// Encodes `rows` under `key` and writes them with `put`, returning how many were written.
async fn publish_partition<K: Into<Key>, Row, Refusal: Into<archive::EncodeRefusal>>(
    put: impl AsyncFnOnce(&Key, Vec<u8>) -> Result<(), ArchiveError>,
    key: K,
    rows: &Partition<Row>,
    provenance: &Provenance,
    encode: impl FnOnce(&K, &Partition<Row>, &Provenance) -> Result<Vec<u8>, Refusal>,
) -> Result<u64, PartitionFailure> {
    let body = encode(&key, rows, provenance).map_err(Into::into)?;
    put(&key.into(), body).await?;
    Ok(u64::try_from(rows.rows().len()).expect("a partition holds fewer than u64::MAX rows"))
}

/// Publishes a fold's minute bars and their rollups under `key` at each interval, the daily last so a session reads as
/// held only once all three are, returning how many minute bars were written.
async fn publish_ticks<K: Into<Key>, B: RollsUp, Refusal: Into<archive::EncodeRefusal>>(
    put: impl AsyncFn(&Key, Vec<u8>) -> Result<(), ArchiveError>,
    minutes: Vec<B>,
    key: impl Fn(BarInterval) -> K,
    provenance: &Provenance,
    encode: impl Fn(&K, &Partition<B>, &Provenance) -> Result<Vec<u8>, Refusal>,
) -> Result<u64, PartitionFailure> {
    let [(interval, minute_bars), five_minute, daily] = session_bars(partition(minutes)?);
    let written = publish_partition(&put, key(interval), &minute_bars, provenance, &encode).await?;
    for (interval, bars) in [five_minute, daily] {
        publish_partition(&put, key(interval), &bars, provenance, &encode).await?;
    }
    Ok(written)
}

/// Writes Massive's grouped daily bars for the session.
async fn write_daily_bars(
    session: SessionDate,
    clients: &Clients,
    journal: &Journal,
) -> Result<PartitionWritten, PartitionFailure> {
    let daily = clients.massive.grouped_daily(session).await?;
    if !daily.test_tickers().is_empty() {
        tracing::info!(%session, test_tickers = daily.test_tickers().len(), "Exchange test tickers left out");
    }
    let provenance = fetched_now(Subscription::StocksStarter, journal);
    let refused = refused_by_cause(daily.refused());
    let bars = publish_partition(
        clients.put(),
        massive_daily_bars(session),
        &partition(daily.into_bars())?,
        &provenance,
        bars::encode,
    )
    .await?;
    Ok(PartitionWritten::new(
        Leg::MassiveDailyBars,
        session,
        bars,
        refused,
        BTreeMap::new(),
        BTreeMap::new(),
    ))
}

/// Writes Alpaca's one-minute bars for every symbol of the session, fetched in concurrent batches.
async fn write_minute_bars(
    session: SessionDate,
    parameters: &Parameters,
    clients: &Clients,
    journal: &Journal,
) -> Result<PartitionWritten, PartitionFailure> {
    let symbols = symbol_list(clients, session).await?;
    let minute: MinuteBars = in_batches(
        &symbols,
        parameters.minute_batch_symbols,
        parameters.minute_concurrency,
        |batch| {
            let alpaca = Arc::clone(&clients.alpaca);
            async move { alpaca.minute_bars(&batch, session).await }
        },
    )
    .await?;
    let unanswered = minute
        .missing()
        .iter()
        .map(|symbol| (symbol.clone(), Unanswered::Missing))
        .chain(
            minute
                .invalid()
                .iter()
                .map(|symbol| (symbol.clone(), Unanswered::Invalid)),
        )
        .collect();
    let provenance = fetched_now(Subscription::AlgoTraderPlus, journal);
    let refused = refused_by_cause(minute.refused());
    let bars = publish_partition(
        clients.put(),
        alpaca_minute_bars(session),
        &partition(minute.into_bars())?,
        &provenance,
        bars::encode,
    )
    .await?;
    Ok(PartitionWritten::new(
        Leg::AlpacaMinuteBars,
        session,
        bars,
        refused,
        unanswered,
        BTreeMap::new(),
    ))
}

/// Rolls the session's written Alpaca minute bars up to five minutes, under the minutes' own provenance.
async fn write_five_minute_bars(
    session: SessionDate,
    clients: &Clients,
) -> Result<PartitionWritten, PartitionFailure> {
    let minutes = clients
        .archive
        .get(&Key::from(alpaca_minute_bars(session)))
        .await?;
    roll_up_minutes(session, minutes, clients.put()).await
}

/// Writes the five-minute bars of `session`'s Alpaca minute file, which is `None` when the archive holds none.
async fn roll_up_minutes(
    session: SessionDate,
    minutes: Option<Vec<u8>>,
    put: impl AsyncFnOnce(&Key, Vec<u8>) -> Result<(), ArchiveError>,
) -> Result<PartitionWritten, PartitionFailure> {
    let minute_key = alpaca_minute_bars(session);
    let body = minutes.ok_or(PartitionFailure::NoMinutes {
        key: minute_key.into(),
    })?;
    let (minutes, provenance) =
        bars::decode(&minute_key, body).map_err(archive::DecodeRefusal::from)?;
    let five_minutes = aggregate::roll_up(minutes.rows(), BarInterval::FiveMinute)
        .expect("minutes roll up to five minutes");
    let five_minutes = Partition::try_from(five_minutes)
        .expect("a minute or more rolls up to a five-minute bar or more");
    let bars = publish_partition(
        put,
        alpaca_five_minute_bars(session),
        &five_minutes,
        &provenance,
        bars::encode,
    )
    .await?;
    Ok(PartitionWritten::new(
        Leg::AlpacaFiveMinuteBars,
        session,
        bars,
        refused_by_cause(&[]),
        BTreeMap::new(),
        BTreeMap::new(),
    ))
}

/// Writes Massive's whole split table as the session's snapshot.
async fn write_splits(
    session: SessionDate,
    clients: &Clients,
    journal: &Journal,
) -> Result<PartitionWritten, PartitionFailure> {
    let provenance = fetched_now(Subscription::StocksStarter, journal);
    let (splits, bodies) = clients.massive.splits().await?.into_parts();
    clients.store()(massive_splits(session), &provenance, bodies).await?;
    let rows = publish(
        clients.put(),
        massive_splits(session),
        splits.splits(),
        &provenance,
        encode_splits,
    )
    .await?;
    Ok(PartitionWritten::new(
        Leg::MassiveSplits,
        session,
        rows,
        refused_by_cause(splits.refused()),
        BTreeMap::new(),
        BTreeMap::new(),
    ))
}

/// How far before the previous snapshot a refresh reaches back, and the span of one request.
const BOUNDARY_REFRESH_DAYS: NonZeroU16 = match NonZeroU16::new(365) {
    Some(days) => days,
    None => panic!("a year is more than no days"),
};

/// Where the first snapshot's history starts, fetched a refresh window at a time.
const BOUNDARIES_SINCE: (i32, u32, u32) = (2015, 1, 1);

/// Writes the session's series boundaries: the previous snapshot refreshed from a year before it was taken, so a
/// missed night leaves no gap, or, with none to refresh, the feed's history; either is read a year at a time.
async fn write_series_boundaries(
    session: SessionDate,
    clients: &Clients,
    journal: &Journal,
) -> Result<PartitionWritten, PartitionFailure> {
    let previous = latest_snapshot(
        &clients.archive,
        Provider::Alpaca,
        ReferenceTable::SeriesBoundaries,
        Some(session),
    )
    .await?;
    let (held, first) = match previous {
        Some(key) => {
            let bytes = clients
                .archive
                .get(&Key::from(key))
                .await?
                .ok_or(PartitionFailure::Vanished { key: key.into() })?;
            let (held, _) =
                decode_series_boundaries(&key, bytes).map_err(archive::DecodeRefusal::Reference)?;
            (
                held,
                key.as_of()
                    .plus_calendar_days(-i64::from(BOUNDARY_REFRESH_DAYS.get())),
            )
        }
        None => {
            let (year, month, day) = BOUNDARIES_SINCE;
            let since = chrono::NaiveDate::from_ymd_opt(year, month, day)
                .expect("the boundaries' first day is a date");
            (Vec::new(), SessionDate::from_date(since))
        }
    };
    let window = SessionRange::single(session).reaching_back_to(first);
    let provenance = fetched_now(Subscription::AlgoTraderPlus, journal);
    let mut read = SeriesBoundaries::empty();
    let mut bodies = Vec::new();
    for span in window.spans(BOUNDARY_REFRESH_DAYS) {
        let (boundaries, pages) = clients.alpaca.series_boundaries(span).await?.into_parts();
        read = read.combine(boundaries);
        bodies.extend(pages);
    }
    let boundaries = refresh_boundaries(&held, read.boundaries(), window);
    clients.store()(alpaca_series_boundaries(session), &provenance, bodies).await?;
    let rows = publish(
        clients.put(),
        alpaca_series_boundaries(session),
        &boundaries,
        &provenance,
        encode_series_boundaries,
    )
    .await?;
    Ok(PartitionWritten::new(
        Leg::AlpacaSeriesBoundaries,
        session,
        rows,
        refused_by_cause(read.refused()),
        BTreeMap::new(),
        BTreeMap::new(),
    ))
}

/// What the details requests for a session answered, gathered across symbols.
#[derive(Debug, Clone, Default, PartialEq)]
struct DetailsGathered {
    details: Vec<SecurityDetails>,
    missing: Vec<Symbol>,
    refused: Vec<RefusedRow>,
    bodies: Vec<Vec<u8>>,
}

impl Monoid for DetailsGathered {
    fn empty() -> Self {
        Self::default()
    }

    fn combine(mut self, other: Self) -> Self {
        self.details.extend(other.details);
        self.missing.extend(other.missing);
        self.refused.extend(other.refused);
        self.bodies.extend(other.bodies);
        self
    }
}

/// Writes the details of every symbol that traded on `session`, as Massive held them on that date.
async fn write_security_details(
    session: SessionDate,
    parameters: &Parameters,
    clients: &Clients,
    journal: &Journal,
) -> Result<PartitionWritten, PartitionFailure> {
    let symbols = symbol_list(clients, session).await?;
    let provenance = fetched_now(Subscription::StocksStarter, journal);
    let one = NonZeroUsize::new(1).expect("one is not zero");
    let gathered: DetailsGathered =
        in_batches(&symbols, one, parameters.tick_concurrency, |batch| {
            let massive = Arc::clone(&clients.massive);
            async move {
                let mut gathered = DetailsGathered::default();
                for symbol in batch {
                    let (answer, bodies) = massive
                        .security_details(&symbol, session)
                        .await?
                        .into_parts();
                    gathered.bodies.extend(bodies);
                    match answer {
                        DetailsAnswer::Details(details) => gathered.details.push(details),
                        DetailsAnswer::Missing => gathered.missing.push(symbol),
                        DetailsAnswer::Refused(row) => gathered.refused.push(row),
                    }
                }
                Ok(gathered)
            }
        })
        .await?;
    clients.store()(
        massive_security_details(session),
        &provenance,
        gathered.bodies,
    )
    .await?;
    let rows = publish(
        clients.put(),
        massive_security_details(session),
        &gathered.details,
        &provenance,
        encode_security_details,
    )
    .await?;
    Ok(PartitionWritten::new(
        Leg::MassiveSecurityDetails,
        session,
        rows,
        refused_by_cause(&gathered.refused),
        gathered
            .missing
            .into_iter()
            .map(|symbol| (symbol, Unanswered::Missing))
            .collect(),
        BTreeMap::new(),
    ))
}

/// Folds Alpaca's quotes for every symbol of the session and writes its quote bars.
async fn write_quotes(
    session: SessionDate,
    (open, close): (DateTime<Utc>, DateTime<Utc>),
    parameters: &Parameters,
    clients: &Clients,
    journal: &Journal,
) -> Result<PartitionWritten, PartitionFailure> {
    let symbols = symbol_list(clients, session).await?;
    let mut fold = QuoteFold::new(open, close).map_err(PartitionFailure::Fold)?;
    let mut refused = Vec::new();
    let mut one_sided = 0_u64;
    let provenance = fetched_now(Subscription::AlgoTraderPlus, journal);
    let unanswered = per_symbol_raw(
        &clients.archive,
        alpaca_quotes(session).into(),
        Key::RawQuotes {
            provider: Provider::Alpaca,
            session,
        },
        &provenance,
        &symbols,
        parameters.tick_concurrency,
        |symbol, pages| {
            let alpaca = Arc::clone(&clients.alpaca);
            async move { alpaca.quotes(&symbol, session, &pages).await }
        },
        |outcome| match outcome {
            AlpacaQuoteOutcome::Quote(quote) => fold.push(&quote),
            AlpacaQuoteOutcome::OneSided => one_sided += 1,
            AlpacaQuoteOutcome::Refused(row) => refused.push(row),
        },
    )
    .await?;
    let (minutes, counts) = fold.finish();
    let minute_bars = publish_ticks(
        clients.put(),
        minutes,
        |interval| alpaca_quotes(session).at_interval(interval),
        &provenance,
        quote_bars::encode,
    )
    .await?;
    tracing::info!(%session, quotes = counts.accepted(), out_of_order = counts.out_of_order(), one_sided, "Alpaca quotes folded");
    Ok(PartitionWritten::new(
        Leg::AlpacaQuotes,
        session,
        minute_bars,
        refused_by_cause(&refused),
        unanswered,
        quotes_folded(counts, one_sided),
    ))
}

/// Folds Alpaca's trades for every symbol of the session under the newest conditions table and writes its trade bars.
async fn write_trades(
    session: SessionDate,
    parameters: &Parameters,
    clients: &Clients,
    journal: &mut Journal,
) -> Result<PartitionWritten, PartitionFailure> {
    let symbols = symbol_list(clients, session).await?;
    let conditions = trade_conditions(clients, journal).await?;
    let mut fold = TradeFold::new(session, conditions);
    let mut refused = Vec::new();
    let provenance = fetched_now(Subscription::AlgoTraderPlus, journal);
    let unanswered = per_symbol_raw(
        &clients.archive,
        alpaca_trades(session).into(),
        Key::RawTrades {
            provider: Provider::Alpaca,
            session,
        },
        &provenance,
        &symbols,
        parameters.tick_concurrency,
        |symbol, pages| {
            let alpaca = Arc::clone(&clients.alpaca);
            async move { alpaca.trades(&symbol, session, &pages).await }
        },
        |outcome| match outcome {
            AlpacaTradeOutcome::Print {
                print,
                tape,
                letters,
                correction,
            } => fold.push_lettered(&print, tape, &letters, correction),
            AlpacaTradeOutcome::Refused(row) => refused.push(row),
        },
    )
    .await?;
    let (minutes, counts) = fold.finish();
    let minute_bars = publish_ticks(
        clients.put(),
        minutes,
        |interval| alpaca_trades(session).at_interval(interval),
        &provenance,
        trade_bars::encode,
    )
    .await?;
    tracing::info!(%session, folded = counts.folded(), withdrawn = counts.withdrawn(), unresolved = counts.unresolved().total(), unresolved_by_cause = %counts.unresolved(), unsized_prints = counts.unsized_prints(), "Alpaca trades folded");
    Ok(PartitionWritten::new(
        Leg::AlpacaTrades,
        session,
        minute_bars,
        refused_by_cause(&refused),
        unanswered,
        trades_folded(&counts),
    ))
}

/// The newest conditions snapshot, or, when none reads under this build's layout, today's fetched from Massive and
/// written and journaled first, so the trade leg never waits on an operator.
async fn trade_conditions(
    clients: &Clients,
    journal: &mut Journal,
) -> Result<TradeConditions, PartitionFailure> {
    match latest_conditions(&clients.archive).await {
        Ok((_, conditions)) => Ok(conditions),
        Err(error) => {
            tracing::warn!(%error, "No readable conditions snapshot; fetching today's");
            write_conditions(
                journal,
                async || clients.massive.trade_conditions().await,
                clients.store(),
                clients.put(),
            )
            .await
        }
    }
}

/// Fetches today's conditions with `fetch`, stores the response with `store`, writes them with `put` and journals the
/// write before returning them.
async fn write_conditions(
    journal: &mut Journal,
    fetch: impl AsyncFnOnce() -> Result<Fetched<TradeConditions>, FetchError>,
    store: impl AsyncFnOnce(ReferenceKey, &Provenance, Vec<Vec<u8>>) -> Result<(), PartitionFailure>,
    put: impl AsyncFnOnce(&Key, Vec<u8>) -> Result<(), ArchiveError>,
) -> Result<TradeConditions, PartitionFailure> {
    let fetched_at = Utc::now();
    let (conditions, bodies) = fetch().await?.into_parts();
    let key = conditions_key(SessionDate::at(fetched_at));
    let provenance = Provenance::new(
        Subscription::StocksStarter,
        fetched_at,
        journal.run_id(),
        journal.commit().cloned(),
    );
    store(key, &provenance, bodies).await?;
    let body =
        encode_conditions(&key, &conditions, &provenance).map_err(archive::EncodeRefusal::from)?;
    put(&key.into(), body).await?;
    let written = ConditionsWritten::new(
        key.as_of(),
        u64::try_from(conditions.conditions().len())
            .expect("a table holds fewer than u64::MAX rows"),
    );
    journal
        .append(Utc::now(), Observation::ConditionsWritten(written))
        .map_err(PartitionFailure::Journal)?;
    Ok(conditions)
}

/// `per_symbol`, storing every page under the raw `key` stamped with `provenance`, the object complete before this
/// returns so nothing folded from the pages is published without them; a failed fetch abandons the upload. `held_by`
/// is deleted first, so a session that fails after its new raw object is written reads as owed rather than held.
#[allow(clippy::too_many_arguments)]
async fn per_symbol_raw<Fetch, Pending, Row>(
    archive: &Archive,
    held_by: Key,
    key: Key,
    provenance: &Provenance,
    symbols: &[Symbol],
    concurrency: NonZeroUsize,
    fetch: Fetch,
    each: impl FnMut(Row),
) -> Result<BTreeMap<Symbol, Unanswered>, PartitionFailure>
where
    Fetch: Fn(Symbol, Sender<Page<Row>>) -> Pending,
    Pending: Future<Output = Result<bool, FetchError>> + Send + 'static,
    Row: Send + 'static,
{
    archive.delete(&held_by).await?;
    let mut writer = archive
        .begin_raw(key, provenance)
        .await
        .map_err(PartitionFailure::Tee)?;
    let fetched = per_symbol(
        symbols,
        concurrency,
        fetch,
        async |body: Vec<u8>| writer.push_line(&body).await,
        each,
    )
    .await;
    match fetched {
        Ok(unanswered) => {
            writer.finish().await.map_err(PartitionFailure::Tee)?;
            Ok(unanswered)
        }
        Err(failure) => {
            writer.abort().await;
            Err(failure)
        }
    }
}

/// Fetches each symbol's rows with at most `concurrency` in flight, handing every page's bytes to `tee` and its rows to
/// `each` as pages arrive, in order within a symbol but interleaved across symbols. A symbol Alpaca names invalid, or
/// answers with no row filed under it, is returned as unanswered rather than failing the session; any other failure
/// fails it.
async fn per_symbol<Fetch, Pending, Row>(
    symbols: &[Symbol],
    concurrency: NonZeroUsize,
    fetch: Fetch,
    mut tee: impl AsyncFnMut(Vec<u8>) -> Result<(), TeeError>,
    mut each: impl FnMut(Row),
) -> Result<BTreeMap<Symbol, Unanswered>, PartitionFailure>
where
    Fetch: Fn(Symbol, Sender<Page<Row>>) -> Pending,
    Pending: Future<Output = Result<bool, FetchError>> + Send + 'static,
    Row: Send + 'static,
{
    let (sender, mut pages) = mpsc::channel(concurrency.get());
    let mut pending = symbols.iter().cloned();
    let mut in_flight = JoinSet::new();
    let mut unanswered = BTreeMap::new();
    loop {
        while in_flight.len() < concurrency.get() {
            let Some(symbol) = pending.next() else {
                break;
            };
            let answer = fetch(symbol.clone(), sender.clone());
            in_flight.spawn(async move { (symbol, answer.await) });
        }
        if in_flight.is_empty() {
            drop(sender);
            while let Some(page) = pages.recv().await {
                let (body, rows) = page.into_parts();
                tee(body).await.map_err(PartitionFailure::Tee)?;
                rows.into_iter().for_each(&mut each);
            }
            return Ok(unanswered);
        }
        tokio::select! {
            Some(page) = pages.recv() => {
                let (body, rows) = page.into_parts();
                tee(body).await.map_err(PartitionFailure::Tee)?;
                rows.into_iter().for_each(&mut each);
            }
            Some(joined) = in_flight.join_next() => {
                let (symbol, answer) =
                    joined.unwrap_or_else(|error| std::panic::resume_unwind(error.into_panic()));
                match answer {
                    Ok(true) => {}
                    Ok(false) => {
                        unanswered.insert(symbol, Unanswered::Missing);
                    }
                    Err(FetchError::Refused { status: 400, body })
                        if invalid_symbol(&body).as_deref() == Some(symbol.as_str()) =>
                    {
                        unanswered.insert(symbol, Unanswered::Invalid);
                    }
                    Err(error) => return Err(error.into()),
                }
            }
        }
    }
}

/// The session's symbols, read from its written daily bars, since those name what traded that day.
async fn symbol_list(
    clients: &Clients,
    session: SessionDate,
) -> Result<Vec<Symbol>, PartitionFailure> {
    let key = massive_daily_bars(session);
    let body = clients
        .archive
        .get(&Key::from(key))
        .await?
        .ok_or(PartitionFailure::NoSymbols { key: key.into() })?;
    let (bars, _) = bars::decode(&key, body).map_err(archive::DecodeRefusal::from)?;
    Ok(bars.rows().iter().map(Bar::symbol).cloned().collect())
}

/// Fetches `symbols` in batches with at most `concurrency` in flight, concatenating the answers in the order the
/// batches were cut, whatever order they finish in; the first batch to fail fails them all and cancels the rest.
async fn in_batches<Fetch, Pending, Answer>(
    symbols: &[Symbol],
    batch_symbols: NonZeroUsize,
    concurrency: NonZeroUsize,
    fetch: Fetch,
) -> Result<Answer, FetchError>
where
    Fetch: Fn(Vec<Symbol>) -> Pending,
    Pending: Future<Output = Result<Answer, FetchError>> + Send + 'static,
    Answer: Monoid + Send + 'static,
{
    let mut batches = symbols
        .chunks(batch_symbols.get())
        .map(<[Symbol]>::to_vec)
        .enumerate();
    let mut answered = BTreeMap::new();
    let mut in_flight = JoinSet::new();
    loop {
        while in_flight.len() < concurrency.get() {
            let Some((index, batch)) = batches.next() else {
                break;
            };
            let pending = fetch(batch);
            in_flight.spawn(async move { (index, pending.await) });
        }
        match in_flight.join_next().await {
            None => return Ok(concatenate(answered.into_values())),
            Some(joined) => {
                let (index, answer) =
                    joined.unwrap_or_else(|error| std::panic::resume_unwind(error.into_panic()));
                answered.insert(index, answer?);
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use std::sync::atomic::{AtomicUsize, Ordering};

    use super::*;
    use crate::common::journal::{ParameterSource, RunId};
    use crate::common::market::trade_bars::{
        Condition, ConditionCode, ConditionStatus, UpdateRules,
    };
    use crate::common::monoid::{Tally, laws};
    use crate::ingest::RowRefusal;

    /// The batches' symbols in the order they were concatenated.
    #[derive(Debug, Clone, PartialEq)]
    struct Names(Vec<String>);

    impl Monoid for Names {
        fn empty() -> Self {
            Self(Vec::new())
        }

        fn combine(mut self, other: Self) -> Self {
            self.0.extend(other.0);
            self
        }
    }

    fn symbols(count: usize) -> Vec<Symbol> {
        (0..count)
            .map(|index| {
                let letters: String = [index / 26, index % 26]
                    .iter()
                    .map(|digit| char::from(b'A' + u8::try_from(*digit).unwrap()))
                    .collect();
                Symbol::new(&letters).unwrap()
            })
            .collect()
    }

    fn size(count: usize) -> NonZeroUsize {
        NonZeroUsize::new(count).unwrap()
    }

    /// The journal keeps a failure's kind beside its text, so a night's failures group without matching on prose.
    #[test]
    fn test_a_failure_is_journaled_with_its_kind_and_its_text() {
        let session = SessionDate::from_date(chrono::NaiveDate::from_ymd_opt(2026, 9, 29).unwrap());
        let outcomes = [
            PartitionFailure::NotInCalendar { session },
            PartitionFailure::NoRows,
            PartitionFailure::Fetch(FetchError::Refused {
                status: 403,
                body: "forbidden".to_string(),
            }),
            PartitionFailure::NoSymbols {
                key: massive_daily_bars(session).into(),
            },
        ]
        .map(|failure| failure.outcome());
        let cause = |failure, cause: &str| SessionOutcome::Failed {
            failure,
            cause: cause.to_string(),
        };
        assert_eq!(
            outcomes,
            [
                cause(
                    PartitionFailureKind::NotInCalendar,
                    "2026-09-29 is not in the calendar"
                ),
                cause(
                    PartitionFailureKind::NoRows,
                    "the vendor answered with no rows"
                ),
                cause(PartitionFailureKind::Fetch, "refused with 403: forbidden"),
                cause(
                    PartitionFailureKind::NoSymbols,
                    "no daily bars at data/equity/stage=parsed/bars/provider=massive/origin=vendor/interval=one_day/\
                     year=2026/month=09/day=29/data.parquet to take the symbols from"
                ),
            ]
        );
    }

    /// The record journaled when a partition fails carries the same kind and text its run's outcome does.
    #[test]
    fn test_a_failure_is_journaled_as_it_happens_with_its_outcome() {
        let session = SessionDate::from_date(chrono::NaiveDate::from_ymd_opt(2026, 9, 29).unwrap());
        let failure = PartitionFailure::Fetch(FetchError::Refused {
            status: 403,
            body: "forbidden".to_string(),
        });
        assert_eq!(
            failure.failed(Leg::AlpacaTrades, session),
            PartitionFailed::new(
                Leg::AlpacaTrades,
                session,
                PartitionFailureKind::Fetch,
                "refused with 403: forbidden".to_string()
            )
        );
        assert_eq!(
            PartitionFailure::Journal(std::io::Error::other("disk full")).kind(),
            PartitionFailureKind::Journal
        );
    }

    /// A journal of its own in a fresh directory, which the caller removes.
    fn temporary_journal() -> (Journal, std::path::PathBuf) {
        let directory = std::env::temp_dir().join(format!("fund-heal-{}", uuid::Uuid::new_v4()));
        let journal = Journal::open(&directory, RunId::new(uuid::Uuid::new_v4())).unwrap();
        (journal, directory)
    }

    fn event_types(journal: &Journal) -> Vec<&'static str> {
        journal
            .history()
            .unwrap()
            .iter()
            .map(|record| record.observation().event_type())
            .collect()
    }

    fn sessions() -> [SessionDate; 2] {
        [29, 30].map(|day| {
            SessionDate::from_date(chrono::NaiveDate::from_ymd_opt(2026, 9, day).unwrap())
        })
    }

    fn far_deadline() -> Instant {
        Instant::now() + Duration::from_secs(3_600)
    }

    /// A failed partition is in the journal before the leg starts its next session, not gathered for the end.
    #[tokio::test]
    async fn test_a_failed_partition_is_journaled_before_the_next_session_starts() {
        let [first, second] = sessions();
        let (mut journal, directory) = temporary_journal();
        let mut seen = Vec::new();
        let outcomes = heal_leg(
            Leg::MassiveDailyBars,
            [first, second],
            far_deadline(),
            &mut journal,
            async |session, journal: &mut Journal| {
                seen.push((session, event_types(journal)));
                if session == first {
                    Err(PartitionFailure::NoRows)
                } else {
                    Ok(PartitionWritten::new(
                        Leg::MassiveDailyBars,
                        session,
                        1,
                        Tally::default(),
                        BTreeMap::new(),
                        BTreeMap::new(),
                    ))
                }
            },
        )
        .await
        .unwrap();
        assert_eq!(seen, [(first, vec![]), (second, vec!["partition_failed"])]);
        assert_eq!(
            event_types(&journal),
            ["partition_failed", "partition_written"]
        );
        assert_eq!(
            outcomes,
            BTreeMap::from([
                (
                    first,
                    SessionOutcome::Failed {
                        failure: PartitionFailureKind::NoRows,
                        cause: "the vendor answered with no rows".to_string(),
                    }
                ),
                (second, SessionOutcome::Written),
            ])
        );
        std::fs::remove_dir_all(&directory).unwrap();
    }

    /// An outcome the journal refuses, or a write the partition could not journal, stops the leg before its next
    /// session.
    #[tokio::test]
    async fn test_a_refused_journal_append_stops_further_partition_work() {
        let [first, second] = sessions();
        let (mut refusing, directory) = temporary_journal();
        std::fs::remove_dir_all(&directory).unwrap();
        let mut attempted = Vec::new();
        let refused = heal_leg(
            Leg::MassiveDailyBars,
            [first, second],
            far_deadline(),
            &mut refusing,
            async |session, _journal: &mut Journal| {
                attempted.push(session);
                Err(PartitionFailure::NoRows)
            },
        )
        .await;
        assert!(matches!(refused, Err(HealError::Journal(_))));
        assert_eq!(attempted, [first]);

        let (mut journal, directory) = temporary_journal();
        let mut attempted = Vec::new();
        let refused = heal_leg(
            Leg::AlpacaTrades,
            [first, second],
            far_deadline(),
            &mut journal,
            async |session, _journal: &mut Journal| {
                attempted.push(session);
                Err(PartitionFailure::Journal(std::io::Error::other(
                    "disk full",
                )))
            },
        )
        .await;
        assert!(matches!(refused, Err(HealError::Journal(_))));
        assert_eq!(attempted, [first]);
        assert_eq!(event_types(&journal), Vec::<&str>::new());
        std::fs::remove_dir_all(&directory).unwrap();
    }

    /// Sessions owed past the deadline are marked unreached without a write or a journal record.
    #[tokio::test]
    async fn test_sessions_past_the_deadline_are_unreached_and_unjournaled() {
        let [first, second] = sessions();
        let (mut journal, directory) = temporary_journal();
        let mut attempted = Vec::new();
        let outcomes = heal_leg(
            Leg::MassiveDailyBars,
            [first, second],
            Instant::now(),
            &mut journal,
            async |session, _journal: &mut Journal| {
                attempted.push(session);
                Err(PartitionFailure::NoRows)
            },
        )
        .await
        .unwrap();
        assert_eq!(attempted, Vec::<SessionDate>::new());
        assert_eq!(
            outcomes,
            BTreeMap::from([
                (first, SessionOutcome::Unreached),
                (second, SessionOutcome::Unreached),
            ])
        );
        assert_eq!(event_types(&journal), Vec::<&str>::new());
        std::fs::remove_dir_all(&directory).unwrap();
    }

    /// A fetched conditions snapshot is stored raw, then written, then journaled, and one never fetched is none of them.
    #[tokio::test]
    async fn test_a_conditions_snapshot_is_journaled_once_it_is_written() {
        let (mut journal, directory) = temporary_journal();
        let order = std::sync::Mutex::new(Vec::new());
        let mut written = Vec::new();
        let conditions = write_conditions(
            &mut journal,
            async || {
                Ok(Fetched::new(
                    TradeConditions::new(BTreeMap::from([(
                        ConditionCode::new(37),
                        Condition::new(
                            UpdateRules::VOLUME_ONLY,
                            None,
                            None,
                            ConditionStatus::Current,
                        ),
                    )])),
                    vec![b"{}".to_vec()],
                ))
            },
            async |key: ReferenceKey, _provenance: &Provenance, bodies: Vec<Vec<u8>>| {
                order
                    .lock()
                    .unwrap()
                    .push((Key::RawReference(key).path(), bodies));
                Ok(())
            },
            async |key: &Key, _body| {
                order.lock().unwrap().push((key.path(), Vec::new()));
                written.push(key.clone());
                Ok(())
            },
        )
        .await
        .unwrap();
        assert_eq!(conditions.conditions().len(), 1);
        assert_eq!(written.len(), 1);
        let as_of = written[0].session();
        assert_eq!(
            order.into_inner().unwrap(),
            [
                (
                    format!(
                        "data/equity/stage=raw/reference/provider=massive/table=conditions/as_of={as_of}/data.jsonl.gz"
                    ),
                    vec![b"{}".to_vec()],
                ),
                (
                    format!(
                        "data/equity/stage=parsed/reference/provider=massive/table=conditions/as_of={as_of}/data.parquet"
                    ),
                    Vec::new(),
                ),
            ]
        );
        let observations: Vec<Observation> = journal
            .history()
            .unwrap()
            .iter()
            .map(|record| record.observation().clone())
            .collect();
        assert_eq!(
            observations,
            [Observation::ConditionsWritten(ConditionsWritten::new(
                written[0].session(),
                1
            ))]
        );

        let (mut journal, unfetched_directory) = temporary_journal();
        let mut written = Vec::new();
        let refused = write_conditions(
            &mut journal,
            async || {
                Err(FetchError::Refused {
                    status: 403,
                    body: "forbidden".to_string(),
                })
            },
            async |_: ReferenceKey, _: &Provenance, _: Vec<Vec<u8>>| {
                panic!("nothing fetched is stored")
            },
            async |key: &Key, _body| {
                written.push(key.clone());
                Ok(())
            },
        )
        .await;
        assert!(matches!(refused, Err(PartitionFailure::Fetch(_))));
        assert_eq!(written, Vec::<Key>::new());
        assert_eq!(event_types(&journal), Vec::<&str>::new());
        std::fs::remove_dir_all(&directory).unwrap();
        std::fs::remove_dir_all(&unfetched_directory).unwrap();
    }

    /// Two prints a minute apart, folded into the minute bars a trade leg publishes.
    fn trade_minutes(session: SessionDate) -> Vec<crate::common::market::trade_bars::TradeBar> {
        use crate::common::market::record::Trade;
        use crate::common::market::trade_bars::{Correction, Print};
        use crate::common::market::{Price, Shares};
        let mut fold = TradeFold::new(session, TradeConditions::new(BTreeMap::new()));
        for at in ["2026-09-29T13:31:00Z", "2026-09-29T13:32:00Z"] {
            let print = Trade::new(
                Symbol::new("AAPL").unwrap(),
                at.parse().unwrap(),
                Price::from_dollars(100.0).unwrap(),
                Shares::whole(10).unwrap(),
            )
            .unwrap();
            fold.push(&Print::Trade(print), &[], Correction::Stands);
        }
        fold.finish().0
    }

    /// A session's three tick files land minute first and daily last, and the count journaled is the minute bars'.
    #[tokio::test]
    async fn test_ticks_publish_each_interval_with_the_daily_last() {
        let [session, _] = sessions();
        let provenance = Provenance::new(
            Subscription::AlgoTraderPlus,
            "2026-09-30T07:00:00Z".parse().unwrap(),
            RunId::new(uuid::Uuid::from_u128(1)),
            None,
        );
        let written = std::sync::Mutex::new(Vec::new());
        let put = async |key: &Key, _body| {
            written.lock().unwrap().push(key.path());
            Ok(())
        };
        let minute_bars = publish_ticks(
            &put,
            trade_minutes(session),
            |interval| alpaca_trades(session).at_interval(interval),
            &provenance,
            trade_bars::encode,
        )
        .await
        .unwrap();
        assert_eq!(minute_bars, 2);
        let prefix = "data/equity/stage=parsed/trades/provider=alpaca/origin=derived";
        let suffix = "year=2026/month=09/day=29/data.parquet";
        assert_eq!(
            *written.lock().unwrap(),
            ["one_minute", "five_minute", "one_day"]
                .map(|interval| format!("{prefix}/interval={interval}/{suffix}"))
        );

        written.lock().unwrap().clear();
        let refused = publish_ticks(
            &put,
            Vec::new(),
            |interval| alpaca_trades(session).at_interval(interval),
            &provenance,
            trade_bars::encode,
        )
        .await;
        assert!(matches!(refused, Err(PartitionFailure::NoRows)));
        assert_eq!(*written.lock().unwrap(), Vec::<String>::new());
    }

    /// The stored minutes land as five-minute bars under the derived key, carrying the minutes' provenance; a session
    /// with no minute file is refused before anything is written.
    #[tokio::test]
    async fn test_minutes_roll_up_to_five_minutes_under_their_own_provenance() {
        use crate::common::market::record::BarPrices;
        use crate::common::market::{Price, Shares, TradeCount};
        let [session, _] = sessions();
        let provenance = Provenance::new(
            Subscription::AlgoTraderPlus,
            "2026-09-30T07:00:00Z".parse().unwrap(),
            RunId::new(uuid::Uuid::from_u128(1)),
            None,
        );
        let bar = |interval, at: &str, [open, high, low, close]: [f64; 4], shares, trades| {
            let price = |dollars| Price::from_dollars(dollars).unwrap();
            Bar::new(
                Symbol::new("AAPL").unwrap(),
                interval,
                at.parse().unwrap(),
                BarPrices::new(price(open), price(high), price(low), price(close)).unwrap(),
                Shares::whole(shares).unwrap(),
                Some(TradeCount::new(trades)),
                None,
            )
            .unwrap()
        };
        let minutes = [
            ("2026-09-29T13:30:00Z", [10.0, 11.0, 9.0, 10.5], 100, 2),
            ("2026-09-29T13:31:00Z", [10.5, 12.0, 10.0, 11.5], 50, 1),
            ("2026-09-29T13:35:00Z", [11.5, 11.5, 11.0, 11.25], 30, 4),
        ]
        .map(|(at, prices, shares, trades)| {
            bar(BarInterval::OneMinute, at, prices, shares, trades)
        });
        let body = bars::encode(
            &alpaca_minute_bars(session),
            &Partition::try_from(minutes.to_vec()).unwrap(),
            &provenance,
        )
        .unwrap();
        let written = std::sync::Mutex::new(Vec::new());
        let put = async |key: &Key, body| {
            written.lock().unwrap().push((key.path(), body));
            Ok(())
        };

        let outcome = roll_up_minutes(session, Some(body), &put).await.unwrap();
        assert_eq!(outcome.leg(), Leg::AlpacaFiveMinuteBars);
        assert_eq!(outcome.rows(), 2);
        let [(path, body)]: [(String, Vec<u8>); 1] = std::mem::take(&mut *written.lock().unwrap())
            .try_into()
            .unwrap();
        assert_eq!(
            path,
            "data/equity/stage=parsed/bars/provider=alpaca/origin=derived/interval=five_minute/year=2026/month=09/day=29/data.parquet"
        );
        let (five_minutes, read_provenance) =
            bars::decode(&alpaca_five_minute_bars(session), body).unwrap();
        assert_eq!(read_provenance, provenance);
        assert_eq!(
            five_minutes.rows(),
            [
                bar(
                    BarInterval::FiveMinute,
                    "2026-09-29T13:30:00Z",
                    [10.0, 12.0, 9.0, 11.5],
                    150,
                    3
                ),
                bar(
                    BarInterval::FiveMinute,
                    "2026-09-29T13:35:00Z",
                    [11.5, 11.5, 11.0, 11.25],
                    30,
                    4
                ),
            ]
        );

        let refused = roll_up_minutes(session, None, &put).await;
        assert!(
            matches!(&refused, Err(PartitionFailure::NoMinutes { key }) if *key == Key::from(alpaca_minute_bars(session))),
            "{refused:?}"
        );
        assert!(written.lock().unwrap().is_empty());
    }

    /// An empty fold is refused before anything is written, so the session is not marked held.
    #[tokio::test]
    async fn test_no_bars_are_refused_unwritten() {
        let [session, _] = sessions();
        let provenance = Provenance::new(
            Subscription::StocksStarter,
            "2026-09-30T07:00:00Z".parse().unwrap(),
            RunId::new(uuid::Uuid::from_u128(1)),
            None,
        );
        let written = std::sync::Mutex::new(Vec::new());
        let put = async |key: &Key, _body| {
            written.lock().unwrap().push(key.path());
            Ok(())
        };
        let refused = publish_ticks(
            &put,
            Vec::<crate::common::market::trade_bars::TradeBar>::new(),
            |interval| alpaca_trades(session).at_interval(interval),
            &provenance,
            trade_bars::encode,
        )
        .await;
        assert!(matches!(refused, Err(PartitionFailure::NoRows)));
        assert_eq!(*written.lock().unwrap(), Vec::<String>::new());
    }

    #[tokio::test(start_paused = true)]
    async fn test_batches_concatenate_in_the_order_cut_whatever_order_they_finish() {
        let names = symbols(25);
        let in_flight = Arc::new(AtomicUsize::new(0));
        let most = Arc::new(AtomicUsize::new(0));
        let answer = in_batches(&names, size(4), size(3), |batch| {
            let (in_flight, most) = (Arc::clone(&in_flight), Arc::clone(&most));
            async move {
                let now = in_flight.fetch_add(1, Ordering::SeqCst) + 1;
                most.fetch_max(now, Ordering::SeqCst);
                // Later batches finish first.
                let first = u64::from(batch[0].as_str().as_bytes()[1]);
                tokio::time::sleep(Duration::from_millis(1_000 - first)).await;
                in_flight.fetch_sub(1, Ordering::SeqCst);
                Ok(Names(batch.iter().map(ToString::to_string).collect()))
            }
        })
        .await
        .unwrap();
        let expected: Vec<String> = names.iter().map(ToString::to_string).collect();
        assert_eq!(answer, Names(expected));
        assert_eq!(most.load(Ordering::SeqCst), 3);
    }

    #[tokio::test(start_paused = true)]
    async fn test_a_failed_batch_fails_the_session_and_cancels_the_rest() {
        let started = Arc::new(AtomicUsize::new(0));
        let answer = in_batches(&symbols(40), size(4), size(2), |batch| {
            let started = Arc::clone(&started);
            async move {
                started.fetch_add(1, Ordering::SeqCst);
                if batch[0].as_str() == "AE" {
                    return Err(FetchError::Malformed {
                        reason: "broken page".to_string(),
                    });
                }
                tokio::time::sleep(Duration::from_secs(1)).await;
                Ok(Names(Vec::new()))
            }
        })
        .await;
        assert_eq!(
            answer,
            Err(FetchError::Malformed {
                reason: "broken page".to_string()
            })
        );
        // The second batch fails at once, so only the two first in flight ever started.
        assert_eq!(started.load(Ordering::SeqCst), 2);
    }

    fn supplied(
        values: &[(Parameter, &str)],
    ) -> impl Fn(Parameter) -> Result<Option<String>, ParameterRefusal> {
        let values: BTreeMap<Parameter, String> = values
            .iter()
            .map(|(parameter, raw)| (*parameter, raw.to_string()))
            .collect();
        move |parameter| Ok(values.get(&parameter).cloned())
    }

    #[test]
    fn test_every_parameter_is_journaled_with_its_source() {
        let (parameters, configuration) =
            Parameters::resolved(&supplied(&[(Parameter::LookbackSessions, "10")])).unwrap();
        assert_eq!(parameters.lookback_sessions.get(), 10);
        assert_eq!(parameters.budget, Duration::from_secs(240 * 60));
        let sources: Vec<(Parameter, &str, ParameterSource)> = configuration
            .parameters()
            .iter()
            .map(|(parameter, resolved)| (*parameter, resolved.value(), resolved.source()))
            .collect();
        assert_eq!(
            sources,
            [
                (
                    Parameter::LookbackSessions,
                    "10",
                    ParameterSource::Environment
                ),
                (Parameter::BudgetMinutes, "240", ParameterSource::Default),
                (
                    Parameter::JournalDirectory,
                    "/var/journal/fund",
                    ParameterSource::Default
                ),
                (
                    Parameter::LogDirectory,
                    "/var/log/fund",
                    ParameterSource::Default
                ),
                (
                    Parameter::MinuteBatchSymbols,
                    "200",
                    ParameterSource::Default
                ),
                (Parameter::MinuteConcurrency, "8", ParameterSource::Default),
                (Parameter::TickConcurrency, "16", ParameterSource::Default),
            ]
        );
    }

    /// Past these, the deadline and the calendar range overflow and the run would panic rather than refuse.
    #[test]
    fn test_a_lookback_or_budget_past_its_most_refuses_to_start() {
        for (parameter, most, past) in [
            (Parameter::LookbackSessions, "1260", "1261"),
            (Parameter::BudgetMinutes, "1440", "1441"),
        ] {
            assert!(Parameters::resolved(&supplied(&[(parameter, most)])).is_ok());
            assert_eq!(
                Parameters::resolved(&supplied(&[(parameter, past)])).map(|_| ()),
                Err(ParameterRefusal::OutOfRange {
                    parameter,
                    value: past.to_string(),
                    most: most.to_string(),
                })
            );
        }
        let today = SessionDate::from_date(chrono::NaiveDate::from_ymd_opt(2026, 9, 30).unwrap());
        let range = fetched_range(today, MAXIMUM_LOOKBACK_SESSIONS);
        assert_eq!(range.first().to_string(), "2019-10-01");
        assert!(
            Instant::now()
                .checked_add(Duration::from_secs(1_440 * 60))
                .is_some()
        );
    }

    #[tokio::test]
    async fn test_an_empty_or_invalid_symbol_is_unanswered_and_the_rest_fold() {
        let symbols: Vec<Symbol> = ["AAA", "BBB", "CCC"]
            .iter()
            .map(|name| Symbol::new(name).unwrap())
            .collect();
        let mut rows = Vec::new();
        let mut bodies = Vec::new();
        let unanswered = per_symbol(
            &symbols,
            size(2),
            |symbol, pages| async move {
                match symbol.as_str() {
                    "AAA" => {
                        for (body, page) in [("a1", vec![1, 2]), ("a2", vec![3])] {
                            pages.send(Page::new(body.into(), page)).await.unwrap();
                        }
                        Ok(true)
                    }
                    // Only rows filed under another ticker: the symbol asked for did not answer.
                    "BBB" => {
                        pages
                            .send(Page::new(b"b1".to_vec(), vec![4]))
                            .await
                            .unwrap();
                        Ok(false)
                    }
                    _ => Err(FetchError::Refused {
                        status: 400,
                        body: format!(r#"{{"message":"invalid symbol: {symbol}"}}"#),
                    }),
                }
            },
            async |body| {
                bodies.push(body);
                Ok(())
            },
            |row: i32| rows.push(row),
        )
        .await
        .unwrap();
        rows.sort();
        assert_eq!(rows, [1, 2, 3, 4]);
        bodies.sort();
        assert_eq!(bodies, [b"a1".to_vec(), b"a2".to_vec(), b"b1".to_vec()]);
        assert_eq!(
            unanswered,
            BTreeMap::from([
                (Symbol::new("BBB").unwrap(), Unanswered::Missing),
                (Symbol::new("CCC").unwrap(), Unanswered::Invalid),
            ])
        );
    }

    #[tokio::test]
    async fn test_pages_fold_while_their_symbol_is_still_fetching() {
        let symbols = [Symbol::new("AAA").unwrap()];
        let mut rows = Vec::new();
        // One page fits the channel, so the fetch finishes only if pages are folded as they arrive.
        let answer = per_symbol(
            &symbols,
            size(1),
            |_, pages| async move {
                for page in 0..5 {
                    pages.send(Page::new(Vec::new(), vec![page])).await.unwrap();
                }
                Ok(true)
            },
            async |_| Ok(()),
            |row: i32| rows.push(row),
        );
        let unanswered = tokio::time::timeout(Duration::from_secs(5), answer)
            .await
            .expect("the fold drains pages while the fetch runs")
            .unwrap();
        assert_eq!(rows, [0, 1, 2, 3, 4]);
        assert!(unanswered.is_empty());
    }

    #[tokio::test]
    async fn test_a_page_not_stored_raw_fails_the_session_before_its_rows_fold() {
        let symbols = [Symbol::new("AAA").unwrap()];
        let mut rows = Vec::new();
        let answer = per_symbol(
            &symbols,
            size(1),
            |_, pages| async move {
                let _ = pages.send(Page::new(b"a1".to_vec(), vec![1])).await;
                Ok(true)
            },
            async |_| {
                Err(TeeError::LineBreak {
                    path: "raw".to_string(),
                })
            },
            |row: i32| rows.push(row),
        )
        .await;
        assert!(matches!(
            answer,
            Err(PartitionFailure::Tee(TeeError::LineBreak { .. }))
        ));
        assert!(rows.is_empty());
    }

    /// Stores Massive's split pages raw in this profile's records bucket, clearing the parsed snapshot first.
    #[tokio::test]
    #[ignore = "fetches from Massive and writes one Deep Archive object to the profile's records bucket, then deletes it"]
    async fn live_split_pages_store_raw_after_clearing_the_snapshot() {
        let session = SessionDate::from_date(chrono::NaiveDate::from_ymd_opt(2026, 10, 9).unwrap());
        let configuration = aws_config::load_from_env().await;
        let clients = Clients::new(
            Archive::records(&configuration).unwrap(),
            Massive::from_environment(reqwest::Client::new()).unwrap(),
            Alpaca::from_environment(reqwest::Client::new()).unwrap(),
        );
        let key = massive_splits(session);
        clients
            .archive
            .put(&Key::from(key), b"an earlier fetch".to_vec())
            .await
            .unwrap();
        let provenance = Provenance::new(
            Subscription::StocksStarter,
            Utc::now(),
            RunId::new(uuid::Uuid::new_v4()),
            None,
        );
        let (splits, bodies) = clients.massive.splits().await.unwrap().into_parts();
        let pages = bodies.len();
        clients.store()(key, &provenance, bodies).await.unwrap();
        let raw = Key::RawReference(key);
        let stored = clients.archive.stored(&raw).await.unwrap().unwrap();
        println!(
            "{} splits, {pages} pages, {} bytes, {}",
            splits.splits().len(),
            stored.length(),
            raw.path()
        );
        clients.archive.delete(&raw).await.unwrap();
        assert_eq!(clients.archive.get(&Key::from(key)).await.unwrap(), None);
        assert!(pages > 1);
        assert_eq!(stored.fetched_at(), Some(provenance.fetched_at()));
    }

    /// Stores five busy symbols' quotes for one session, enough to span several parts, in this profile's records bucket,
    /// clearing the earlier fetch's daily fold first.
    #[tokio::test]
    #[ignore = "fetches from Alpaca and writes one Deep Archive object to the profile's records bucket, then deletes it"]
    async fn live_raw_quote_pages_store_across_parts() {
        let session = SessionDate::from_date(chrono::NaiveDate::from_ymd_opt(2026, 10, 9).unwrap());
        let symbols: Vec<Symbol> = ["SPY", "QQQ", "AAPL", "NVDA", "TSLA"]
            .iter()
            .map(|name| Symbol::new(name).unwrap())
            .collect();
        let configuration = aws_config::load_from_env().await;
        let archive = Archive::records(&configuration).unwrap();
        let alpaca = Arc::new(Alpaca::from_environment(reqwest::Client::new()).unwrap());
        let key = Key::RawQuotes {
            provider: Provider::Alpaca,
            session,
        };
        let provenance = Provenance::new(
            Subscription::AlgoTraderPlus,
            Utc::now(),
            RunId::new(uuid::Uuid::new_v4()),
            None,
        );
        let held_by: Key = alpaca_quotes(session).into();
        archive
            .put(&held_by, b"an earlier fetch".to_vec())
            .await
            .unwrap();
        let mut quotes = 0_u64;
        let unanswered = per_symbol_raw(
            &archive,
            held_by.clone(),
            key.clone(),
            &provenance,
            &symbols,
            size(5),
            |symbol, pages| {
                let alpaca = Arc::clone(&alpaca);
                async move { alpaca.quotes(&symbol, session, &pages).await }
            },
            |_| quotes += 1,
        )
        .await
        .unwrap();
        let stored = archive.stored(&key).await.unwrap().unwrap();
        println!(
            "{quotes} quotes, {} bytes, {:?}, {}",
            stored.length(),
            stored.checksum(),
            key.path()
        );
        archive.delete(&key).await.unwrap();
        assert_eq!(archive.get(&held_by).await.unwrap(), None);
        assert!(unanswered.is_empty());
        assert!(stored.length() > 64 * 1024 * 1024);
        assert!(matches!(
            stored.checksum(),
            crate::archive::raw::Checksum::FullObject(_)
        ));
        assert_eq!(stored.fetched_at(), Some(provenance.fetched_at()));
    }

    /// Built field by field rather than through `combine`, so the law test does not lean on what it checks.
    fn any_gathered() -> impl proptest::strategy::Strategy<Value = DetailsGathered> {
        use proptest::prelude::*;
        let symbol = || {
            prop::sample::select(vec!["AAA", "BBB", "CCC"])
                .prop_map(|name| Symbol::new(name).unwrap())
        };
        let refusal = prop::sample::select(vec![RowRefusal::Duplicate, RowRefusal::Unrequested]);
        (
            prop::collection::vec(symbol(), 0..4),
            prop::collection::vec(symbol(), 0..4),
            prop::collection::vec(("[A-Z]{1,3}", refusal), 0..4),
        )
            .prop_map(|(details, missing, refused)| DetailsGathered {
                details: details
                    .into_iter()
                    .map(|symbol| {
                        SecurityDetails::new(symbol, None, None, None, None, None, None, None)
                    })
                    .collect(),
                missing,
                bodies: Vec::new(),
                refused: refused
                    .into_iter()
                    .map(|(ticker, cause)| RefusedRow::new(&ticker, cause))
                    .collect(),
            })
    }

    proptest::proptest! {
        /// Batches gather in the order they were asked, whatever grouping the concurrency happened to merge them in.
        #[test]
        fn property_gathered_details_concatenate_as_a_monoid(
            first in any_gathered(),
            second in any_gathered(),
            third in any_gathered(),
        ) {
            laws::check_ordered(first, second, third)?;
        }
    }

    #[tokio::test]
    async fn test_no_symbols_is_the_empty_answer() {
        let answer = in_batches(&[], size(4), size(2), |_| async {
            Err::<Names, _>(FetchError::Malformed {
                reason: "fetched nothing".to_string(),
            })
        })
        .await;
        assert_eq!(answer, Ok(Names(Vec::new())));
    }
}
