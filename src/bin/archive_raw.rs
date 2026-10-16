//! Massive's flat files into the archive while the Advanced keys last: `copy`, `fold-quotes`, `fold-trades`,
//! `fetch-conditions` and `delete`, each object journaled and shipped. Exits 0 when everything succeeded and
//! shipped, 1 when not, 2 when the run could not start.

use std::collections::{BTreeMap, BTreeSet};
use std::num::NonZeroUsize;
use std::process::ExitCode;
use std::sync::{Arc, Mutex, PoisonError};

use chrono::{DateTime, NaiveDate, Utc};
use strum::IntoEnumIterator;
use tokio::sync::Semaphore;
use tokio::task::JoinSet;
use tracing::Instrument;

use fund::archive::provenance::{Provenance, Subscription};
use fund::archive::raw::CopyError;
use fund::archive::{self, Archive, ArchiveError};
use fund::archive::{quote_bars, reference, trade_bars};
use fund::common::journal::{ObjectDeleted, ObjectWritten, Observation};
use fund::common::market::aggregate::session_bars;
use fund::common::market::quote_bars::QuoteFold;
use fund::common::market::record::{BarInterval, Partition, PartitionRefusal};
use fund::common::market::trade_bars::{TradeConditions, TradeFold};
use fund::common::monoid::{Monoid, Tally};
use fund::common::storage::{
    Family, Host, Key, ParsedKey, Provider, QuotesKey, Service, TradesKey,
};
use fund::common::time::calendar::TradingCalendar;
use fund::common::time::{SessionDate, SessionRange};
use fund::ingest::RowRefusalKind;
use fund::ingest::alpaca::Alpaca;
use fund::ingest::flat_files::{
    FlatFileDataset, FlatFileStream, FlatFiles, ParseRefusal, QuoteRowOutcome, TradeRowOutcome,
    read_quotes, read_trades,
};
use fund::ingest::massive::Massive;
use fund::journal::Journal;
use fund::parameter::Directories;
use fund::records::{Exclusion, RefusedToStart, resolved, ship_logged, start};

const SERVICE: &str = "archive_raw";

/// Sessions named in one log line; the count beside them says how many there are.
const LISTED_SESSIONS: usize = 50;

/// Ranged reads queued ahead of the parser per streamed file; at sixteen mebibytes each, roughly the bytes held ahead of it.
const STREAM_AHEAD: usize = 8;

/// The subcommand named first on the command line.
#[derive(Debug, Clone, Copy, PartialEq, Eq, strum::Display, strum::EnumString, strum::EnumIter)]
#[strum(serialize_all = "kebab-case")]
enum CommandName {
    Copy,
    FoldQuotes,
    FetchConditions,
    Delete,
    FoldTrades,
}

impl CommandName {
    /// The arguments the subcommand takes after its name, as the usage line shows them.
    fn arguments(self) -> &'static str {
        match self {
            Self::Copy => " <dataset> <first> <last> <concurrency>",
            Self::FoldQuotes | Self::FoldTrades => " <first> <last> <concurrency>",
            Self::FetchConditions => "",
            Self::Delete => " <path>...",
        }
    }
}

enum Command {
    Copy {
        dataset: FlatFileDataset,
        range: SessionRange,
        concurrency: NonZeroUsize,
    },
    FoldQuotes {
        range: SessionRange,
        concurrency: NonZeroUsize,
    },
    FetchConditions,
    /// Deletes the objects at paths that each parse as a key.
    Delete {
        keys: Vec<Key>,
    },
    FoldTrades {
        range: SessionRange,
        concurrency: NonZeroUsize,
    },
}

fn parse(arguments: &[String]) -> Option<Command> {
    let date = |raw: &str| {
        NaiveDate::parse_from_str(raw, "%Y-%m-%d")
            .ok()
            .map(SessionDate::from_date)
    };
    let range = |first: &str, last: &str| SessionRange::new(date(first)?, date(last)?).ok();
    let concurrency = |raw: &str| raw.parse::<NonZeroUsize>().ok();
    let ranged = |rest: &[String]| {
        let [first, last, count] = rest else {
            return None;
        };
        Some((range(first, last)?, concurrency(count)?))
    };
    let (name, rest) = arguments.split_first()?;
    match name.parse::<CommandName>().ok()? {
        CommandName::Copy => {
            let [dataset, first, last, count] = rest else {
                return None;
            };
            Some(Command::Copy {
                dataset: dataset.parse().ok()?,
                range: range(first, last)?,
                concurrency: concurrency(count)?,
            })
        }
        CommandName::FetchConditions => rest.is_empty().then_some(Command::FetchConditions),
        CommandName::Delete => {
            let keys = rest
                .iter()
                .map(|path| path.parse::<Key>().ok())
                .collect::<Option<Vec<_>>>()?;
            (!keys.is_empty()).then_some(Command::Delete { keys })
        }
        CommandName::FoldTrades => {
            ranged(rest).map(|(range, concurrency)| Command::FoldTrades { range, concurrency })
        }
        CommandName::FoldQuotes => {
            ranged(rest).map(|(range, concurrency)| Command::FoldQuotes { range, concurrency })
        }
    }
}

/// Every subcommand with its arguments, one alternative each.
fn usage() -> String {
    CommandName::iter()
        .map(|name| format!("{name}{}", name.arguments()))
        .collect::<Vec<_>>()
        .join(" | ")
}

/// The run's journal, shared by the concurrent sessions so each object is recorded as it lands.
type SharedJournal = Arc<Mutex<Journal>>;

/// Journals `observation` now.
fn journaled(journal: &Mutex<Journal>, observation: Observation) -> std::io::Result<()> {
    journal
        .lock()
        .unwrap_or_else(PoisonError::into_inner)
        .append(Utc::now(), observation)
}

/// Journals the object of `bytes` written under `key`.
fn written(journal: &Mutex<Journal>, key: Key, bytes: u64) -> std::io::Result<()> {
    journaled(
        journal,
        Observation::ObjectWritten(ObjectWritten::new(key, bytes)),
    )
}

/// A body's length as the journal counts it.
fn length(body: &[u8]) -> u64 {
    u64::try_from(body.len()).expect("a body holds fewer than u64::MAX bytes")
}

/// The provenance of rows `subscription` answered at `fetched_at`, written by this run.
fn provenance(
    subscription: Subscription,
    fetched_at: DateTime<Utc>,
    journal: &Mutex<Journal>,
) -> Provenance {
    let journal = journal.lock().unwrap_or_else(PoisonError::into_inner);
    Provenance::new(
        subscription,
        fetched_at,
        journal.run_id(),
        journal.commit().cloned(),
    )
}

#[tokio::main]
async fn main() -> ExitCode {
    let service = Service::new(SERVICE).expect("the service name is one path segment");
    let parameters = Directories::from_environment();
    let started = start(service.clone(), SessionDate::at(Utc::now()));
    let span = started.span();
    async move {
        let arguments: Vec<String> = std::env::args().skip(1).collect();
        let Some(command) = parse(&arguments) else {
            tracing::error!(?arguments, usage = usage(), "Usage refused");
            return RefusedToStart.into();
        };
        let (directories, configuration) = match resolved(parameters) {
            Ok(resolved) => resolved,
            Err(refused) => return refused.into(),
        };
        // Several commands may run at once; each only creates or deletes the keys it names.
        let journal =
            match started.open(directories.journal(), configuration, Exclusion::Concurrent) {
                Ok((journal, _)) => Arc::new(Mutex::new(journal)),
                Err(refused) => return refused.into(),
            };
        let sdk_configuration = aws_config::load_from_env().await;
        let (archive, records) = match (
            Archive::market_data(&sdk_configuration),
            Archive::records(&sdk_configuration),
        ) {
            (Ok(archive), Ok(records)) => (archive, records),
            (Err(refusal), _) | (_, Err(refusal)) => {
                tracing::error!(%refusal, "Archive configuration refused");
                return RefusedToStart.into();
            }
        };
        let outcome = run(command, archive, &journal).await;
        let all_shipped = ship_logged(
            &records,
            Host::Archiver,
            &service,
            directories.journal(),
            directories.log(),
        )
        .await;
        match (outcome == ExitCode::SUCCESS, all_shipped) {
            (true, false) => ExitCode::FAILURE,
            (true, true) | (false, true | false) => outcome,
        }
    }
    .instrument(span)
    .await
}

/// Runs one command, journaling every object it writes or deletes.
async fn run(command: Command, archive: Archive, journal: &SharedJournal) -> ExitCode {
    match command {
        Command::Copy {
            dataset,
            range,
            concurrency,
        } => {
            let flat_files = match FlatFiles::from_environment() {
                Ok(flat_files) => flat_files,
                Err(refusal) => {
                    tracing::error!(%refusal, "Flat-file configuration refused");
                    return RefusedToStart.into();
                }
            };
            copy(&archive, &flat_files, dataset, range, concurrency, journal).await
        }
        Command::Delete { keys } => delete(&archive, &keys, journal).await,
        Command::FetchConditions => match Massive::from_environment(reqwest::Client::new()) {
            Ok(massive) => fetch_conditions(&archive, &massive, journal).await,
            Err(refusal) => {
                tracing::error!(%refusal, "Client configuration refused");
                RefusedToStart.into()
            }
        },
        Command::FoldTrades { range, concurrency } => {
            let Some((flat_files, alpaca)) = fold_clients() else {
                return RefusedToStart.into();
            };
            fold_trades(&archive, &flat_files, &alpaca, range, concurrency, journal).await
        }
        Command::FoldQuotes { range, concurrency } => {
            let Some((flat_files, alpaca)) = fold_clients() else {
                return RefusedToStart.into();
            };
            fold_quotes(&archive, &flat_files, &alpaca, range, concurrency, journal).await
        }
    }
}

/// Deletes each key in turn, journaling each deletion as it lands.
async fn delete(archive: &Archive, keys: &[Key], journal: &Mutex<Journal>) -> ExitCode {
    let mut failed = 0;
    for key in keys {
        match archive.delete(key).await {
            Ok(()) => {
                tracing::info!(path = key.path(), "Deleted an object");
                let deleted = Observation::ObjectDeleted(ObjectDeleted::new(key.clone()));
                if let Err(error) = journaled(journal, deleted) {
                    failed += 1;
                    tracing::error!(path = key.path(), %error, "Deletion not journaled");
                }
            }
            Err(error) => {
                failed += 1;
                tracing::error!(%error, "Object not deleted");
            }
        }
    }
    if failed == 0 {
        ExitCode::SUCCESS
    } else {
        ExitCode::FAILURE
    }
}

/// Why a raw file was not copied and journaled.
#[derive(Debug, thiserror::Error)]
enum CopyFailure {
    #[error("{0}")]
    Copy(CopyError),
    /// Copied, but the write went unrecorded.
    #[error("journaling the copy failed: {0}")]
    Journal(std::io::Error),
}

async fn copy(
    archive: &Archive,
    flat_files: &FlatFiles,
    dataset: FlatFileDataset,
    range: SessionRange,
    concurrency: NonZeroUsize,
    journal: &SharedJournal,
) -> ExitCode {
    let listing = match flat_files.listing(dataset).await {
        Ok(listing) => listing,
        Err(error) => {
            tracing::error!(%error, "Flat files not listed");
            return ExitCode::FAILURE;
        }
    };
    let held = match held(archive, dataset).await {
        Ok(held) => held,
        Err(error) => {
            tracing::error!(%error, "Archive not listed");
            return ExitCode::FAILURE;
        }
    };
    let offered: Vec<_> = listing
        .into_iter()
        .filter(|listed| range.contains(listed.session()))
        .collect();
    let owed: Vec<_> = offered
        .iter()
        .filter(|listed| !held.contains(&listed.session()))
        .cloned()
        .collect();
    tracing::info!(
        %dataset,
        first = %range.first(),
        last = %range.last(),
        offered = offered.len(),
        offered_first = ?offered.first().map(|listed| listed.session().to_string()),
        offered_last = ?offered.last().map(|listed| listed.session().to_string()),
        held = offered.len() - owed.len(),
        owed = owed.len(),
        owed_bytes = owed.iter().map(|listed| listed.length()).sum::<u64>(),
        concurrency,
        series = %dataset.key(range.first()).series(),
        "Planned a raw copy"
    );
    let permits = Arc::new(Semaphore::new(concurrency.get()));
    let mut tasks = JoinSet::new();
    let mut outcomes = BTreeMap::new();
    let mut panicked = 0;
    for listed in owed {
        // At most `concurrency` sessions are in flight, so small files overlap while a large one fills every permit.
        while tasks.len() >= concurrency.get() {
            if let Some(joined) = tasks.join_next().await {
                record(joined, &mut outcomes, &mut panicked);
            }
        }
        let archive = archive.clone();
        let flat_files = flat_files.clone();
        let permits = Arc::clone(&permits);
        let journal = Arc::clone(journal);
        tasks.spawn(
            async move {
                let started = tokio::time::Instant::now();
                let provenance = provenance(Subscription::StocksAdvanced, Utc::now(), &journal);
                let outcome = match archive
                    .copy_flat_file(&flat_files, dataset, &listed, &provenance, permits)
                    .await
                {
                    Ok(stored) => written(&journal, dataset.key(listed.session()), stored.length())
                        .map(|()| stored)
                        .map_err(CopyFailure::Journal),
                    Err(error) => Err(CopyFailure::Copy(error)),
                };
                let seconds = started.elapsed().as_secs_f64();
                match &outcome {
                    Ok(stored) => tracing::info!(
                        session = %listed.session(),
                        bytes = stored.length(),
                        seconds,
                        megabytes_per_second = stored.length() as f64 / 1e6 / seconds,
                        "Copied a raw file"
                    ),
                    Err(error @ CopyFailure::Copy(_)) => {
                        tracing::error!(session = %listed.session(), %error, "Raw file not copied")
                    }
                    Err(error @ CopyFailure::Journal(_)) => tracing::error!(
                        session = %listed.session(),
                        %error,
                        "Raw file copied but not journaled"
                    ),
                }
                (listed.session(), outcome)
            }
            .in_current_span(),
        );
    }
    while let Some(joined) = tasks.join_next().await {
        record(joined, &mut outcomes, &mut panicked);
    }
    let failed: Vec<String> = outcomes
        .iter()
        .filter(|(_, outcome)| outcome.is_err())
        .map(|(session, _)| session.to_string())
        .collect();
    let contended = outcomes
        .values()
        .filter(|outcome| {
            matches!(
                outcome,
                Err(CopyFailure::Copy(CopyError::Archive(
                    ArchiveError::Contended { .. }
                )))
            )
        })
        .count();
    tracing::info!(
        %dataset,
        copied = outcomes.len() - failed.len(),
        failed = failed.len(),
        contended,
        panicked,
        failed_sessions = failed.join(","),
        "Finished a raw copy"
    );
    if failed.is_empty() && panicked == 0 {
        ExitCode::SUCCESS
    } else {
        ExitCode::FAILURE
    }
}

/// The sessions the raw stage holds for `dataset`.
async fn held(
    archive: &Archive,
    dataset: FlatFileDataset,
) -> Result<BTreeSet<SessionDate>, ArchiveError> {
    let series = dataset.key(SessionDate::from_date(NaiveDate::MIN)).series();
    Ok(archive
        .list(&series)
        .await?
        .iter()
        .filter_map(|path| path.parse::<Key>().ok())
        .map(|key| key.session())
        .collect())
}

fn record<T>(
    joined: Result<(SessionDate, T), tokio::task::JoinError>,
    outcomes: &mut BTreeMap<SessionDate, T>,
    panicked: &mut usize,
) {
    match joined {
        Ok((session, outcome)) => {
            outcomes.insert(session, outcome);
        }
        Err(error) => {
            tracing::error!(%error, "Task failed");
            *panicked += 1;
        }
    }
}

/// The first sessions of `sessions`, comma-joined for one log field.
fn listed(sessions: &[SessionDate]) -> String {
    sessions
        .iter()
        .take(LISTED_SESSIONS)
        .map(ToString::to_string)
        .collect::<Vec<_>>()
        .join(",")
}

/// Where a session's quote or trade bars folded from Massive's flat files at `interval` are written.
fn massive_derived<F: Family>(interval: BarInterval, session: SessionDate) -> ParsedKey<F> {
    ParsedKey::new(Provider::Massive, interval, session)
}

/// The flat files to stream and the calendar to plan by, or `None` once the refusal is logged.
fn fold_clients() -> Option<(FlatFiles, Alpaca)> {
    match (
        FlatFiles::from_environment(),
        Alpaca::from_environment(reqwest::Client::new()),
    ) {
        (Ok(flat_files), Ok(alpaca)) => Some((flat_files, alpaca)),
        (Err(refusal), _) | (_, Err(refusal)) => {
            tracing::error!(%refusal, "Client configuration refused");
            None
        }
    }
}

/// Why a session's quotes or trades were not folded and written.
#[derive(Debug, thiserror::Error)]
enum FoldFailure {
    #[error("{0}")]
    Parse(ParseRefusal),
    #[error("{0}")]
    Encode(archive::EncodeRefusal),
    #[error("{0}")]
    Archive(ArchiveError),
    #[error("the fold did not finish: {0}")]
    Interrupted(String),
    #[error("nothing the fold could keep fell in the session")]
    Empty,
    /// An earlier run wrote different bars under the key, which this run will not replace.
    #[error("{} already holds different bars", .key.path())]
    Held { key: Key },
    /// An earlier run wrote the key, and what it holds does not read back as bars.
    #[error("{} held but unreadable: {refusal}", .key.path())]
    Unreadable {
        key: Key,
        refusal: Box<archive::DecodeRefusal>,
    },
    /// Written, but the write went unrecorded.
    #[error("journaling a write failed: {0}")]
    Journal(std::io::Error),
}

/// Streams each listed `dataset` file of a trading session in `range` whose daily file under `daily` is not
/// yet written, and hands it to `fold`; `noun` names the bars in the log.
#[allow(clippy::too_many_arguments)]
async fn fold_sessions<Fold, Folding>(
    archive: &Archive,
    flat_files: &FlatFiles,
    calendar: &TradingCalendar,
    dataset: FlatFileDataset,
    daily: Key,
    range: SessionRange,
    concurrency: NonZeroUsize,
    noun: &'static str,
    mut fold: Fold,
) -> ExitCode
where
    Fold: FnMut(SessionDate, FlatFileStream) -> Folding,
    Folding: Future<Output = Result<(), FoldFailure>> + Send + 'static,
{
    let (listing, written) = match (
        flat_files.listing(dataset).await,
        archive.list(&daily.series()).await,
    ) {
        (Ok(listing), Ok(written)) => (listing, written),
        (Err(error), _) => {
            tracing::error!(%error, "Flat files not listed");
            return ExitCode::FAILURE;
        }
        (_, Err(error)) => {
            tracing::error!(%error, "Archive not listed");
            return ExitCode::FAILURE;
        }
    };
    // The daily file is written last, so a session holding it holds all three.
    let done: BTreeSet<SessionDate> = written
        .iter()
        .filter_map(|path| path.parse::<Key>().ok())
        .map(|key| key.session())
        .collect();
    let offered: Vec<_> = listing
        .into_iter()
        .filter(|listed| range.contains(listed.session()))
        .collect();
    let untraded: Vec<SessionDate> = offered
        .iter()
        .map(|listed| listed.session())
        .filter(|session| !calendar.is_trading_day(*session))
        .collect();
    let owed: Vec<_> = offered
        .iter()
        .filter(|listed| {
            calendar.is_trading_day(listed.session()) && !done.contains(&listed.session())
        })
        .cloned()
        .collect();
    tracing::info!(
        first = %range.first(),
        last = %range.last(),
        offered = offered.len(),
        already_folded = offered.len() - owed.len() - untraded.len(),
        not_trading_days = listed(&untraded),
        owed = owed.len(),
        owed_bytes = owed.iter().map(|listed| listed.length()).sum::<u64>(),
        concurrency,
        "Planned a {noun} fold"
    );
    let mut tasks = JoinSet::new();
    let mut outcomes = BTreeMap::new();
    let mut panicked = 0;
    for listed_file in owed {
        while tasks.len() >= concurrency.get() {
            if let Some(joined) = tasks.join_next().await {
                record(joined, &mut outcomes, &mut panicked);
            }
        }
        let session = listed_file.session();
        let folding = fold(
            session,
            flat_files.stream(dataset, &listed_file, STREAM_AHEAD),
        );
        tasks.spawn(
            async move {
                let started = tokio::time::Instant::now();
                let outcome = folding.await;
                match &outcome {
                    Ok(()) => tracing::info!(
                        session = %session,
                        seconds = started.elapsed().as_secs_f64(),
                        "Folded a {noun} file"
                    ),
                    Err(failure) => {
                        tracing::error!(session = %session, %failure, bars = noun, "File not folded")
                    }
                }
                (session, outcome)
            }
            .in_current_span(),
        );
    }
    while let Some(joined) = tasks.join_next().await {
        record(joined, &mut outcomes, &mut panicked);
    }
    let failed: Vec<SessionDate> = outcomes
        .iter()
        .filter(|(_, outcome)| outcome.is_err())
        .map(|(session, _)| *session)
        .collect();
    tracing::info!(
        written = outcomes.len() - failed.len(),
        failed = failed.len(),
        failed_sessions = listed(&failed),
        panicked,
        "Finished a {noun} fold"
    );
    if failed.is_empty() && panicked == 0 {
        ExitCode::SUCCESS
    } else {
        ExitCode::FAILURE
    }
}

/// Streams each listed quote file not yet folded from Massive and writes its one-minute, five-minute and daily bars.
async fn fold_quotes(
    archive: &Archive,
    flat_files: &FlatFiles,
    alpaca: &Alpaca,
    range: SessionRange,
    concurrency: NonZeroUsize,
    journal: &SharedJournal,
) -> ExitCode {
    let calendar = match alpaca.calendar(range).await {
        Ok(calendar) => calendar,
        Err(error) => {
            tracing::error!(%error, "Calendar not fetched");
            return ExitCode::FAILURE;
        }
    };
    let daily: QuotesKey = massive_derived(BarInterval::OneDay, range.first());
    fold_sessions(
        archive,
        flat_files,
        &calendar,
        FlatFileDataset::Quotes,
        daily.into(),
        range,
        concurrency,
        "quote",
        |session, stream| {
            let hours = calendar
                .session(session)
                .map(|trading| trading.hours())
                .expect("owed sessions are trading days");
            let archive = archive.clone();
            let journal = Arc::clone(journal);
            let provenance = provenance(Subscription::StocksAdvanced, Utc::now(), &journal);
            async move {
                fold_quotes_one(&archive, stream, session, hours, &provenance, &journal).await
            }
        },
    )
    .await
}
/// Folds one streamed quote file and creates its three files, the daily last.
async fn fold_quotes_one(
    archive: &Archive,
    stream: FlatFileStream,
    session: SessionDate,
    (open, close): (DateTime<Utc>, DateTime<Utc>),
    provenance: &Provenance,
    journal: &Mutex<Journal>,
) -> Result<(), FoldFailure> {
    let folded = tokio::task::spawn_blocking(move || {
        let mut fold =
            QuoteFold::new(open, close).expect("a trading session closes after it opens");
        let mut rows = QuoteRowCounts::default();
        read_quotes(stream, |outcome| match outcome {
            QuoteRowOutcome::Quote { quote, .. } => fold.push(&quote),
            QuoteRowOutcome::TestTicker => rows.test_tickers += 1,
            QuoteRowOutcome::OneSided { .. } => rows.one_sided += 1,
            QuoteRowOutcome::Refused(row) => rows.refused.add(row.cause().kind()),
        })
        .map_err(FoldFailure::Parse)?;
        let (minutes, counts) = fold.finish();
        Ok::<_, FoldFailure>((minutes, counts, rows))
    })
    .await
    .map_err(|error| FoldFailure::Interrupted(error.to_string()))??;
    let (minutes, counts, rows) = folded;
    let minutes = Partition::try_from(minutes).map_err(|refusal| match refusal {
        PartitionRefusal::Empty => FoldFailure::Empty,
    })?;
    let minute_bars = minutes.rows().len();
    let files = session_bars(minutes);
    let symbols = files[2].1.rows().len();
    for (interval, bars) in files {
        let key = massive_derived(interval, session);
        let body = quote_bars::encode(&key, &bars, provenance)
            .map_err(|refusal| FoldFailure::Encode(refusal.into()))?;
        create_or_confirm(archive, &key.into(), body, journal, |held| {
            quote_bars::decode(&key, held).map(|(held, _)| held == bars)
        })
        .await?;
    }
    tracing::info!(
        session = %session,
        symbols,
        minute_bars,
        quotes = counts.accepted(),
        out_of_order = counts.out_of_order(),
        one_sided = rows.one_sided,
        test_tickers = rows.test_tickers,
        refused = rows.refused.total(),
        refused_by_cause = %rows.refused,
        "Wrote quote bars"
    );
    Ok(())
}

/// Creates `key` and journals it, or, when an interrupted run already wrote it, accepts it only if it holds the same
/// bars, so a rerun finishes a session's missing files without replacing what it cannot tell is identical.
async fn create_or_confirm<Refusal: Into<archive::DecodeRefusal>>(
    archive: &Archive,
    key: &Key,
    body: Vec<u8>,
    journal: &Mutex<Journal>,
    same_bars: impl FnOnce(Vec<u8>) -> Result<bool, Refusal>,
) -> Result<(), FoldFailure> {
    let bytes = length(&body);
    match archive.create(key, body).await {
        Ok(()) => written(journal, key.clone(), bytes).map_err(FoldFailure::Journal),
        Err(ArchiveError::Contended { path }) => {
            let held = archive
                .get(key)
                .await
                .map_err(FoldFailure::Archive)?
                .ok_or_else(|| {
                    FoldFailure::Archive(ArchiveError::Contended { path: path.clone() })
                })?;
            match same_bars(held) {
                Ok(true) => {
                    tracing::info!(path, "Kept bars an earlier run wrote identically");
                    Ok(())
                }
                Ok(false) => Err(FoldFailure::Held { key: key.clone() }),
                Err(refusal) => Err(FoldFailure::Unreadable {
                    key: key.clone(),
                    refusal: Box::new(refusal.into()),
                }),
            }
        }
        Err(error) => Err(FoldFailure::Archive(error)),
    }
}

/// Rows of a quote file that did not become a quote, by what they were.
#[derive(Default)]
struct QuoteRowCounts {
    test_tickers: u64,
    one_sided: u64,
    /// Counted by cause rather than kept, since a session refuses tens of thousands of rows.
    refused: Tally<RowRefusalKind>,
}

/// Fetches Massive's condition table and writes it as today's snapshot.
async fn fetch_conditions(
    archive: &Archive,
    massive: &Massive,
    journal: &Mutex<Journal>,
) -> ExitCode {
    let fetched_at = Utc::now();
    let (conditions, bodies) = massive.trade_conditions().await.into_parts();
    let key = reference::conditions_key(SessionDate::at(fetched_at));
    let object = Key::from(key);
    let provenance = provenance(Subscription::StocksStarter, fetched_at, journal);
    match bodies.is_empty() {
        true => {}
        false => match archive.store_responses(key, &provenance, &bodies).await {
            Ok(_) => {}
            Err(error) => {
                tracing::error!(%error, "Conditions responses not stored");
                return ExitCode::FAILURE;
            }
        },
    }
    let conditions = match conditions {
        Ok(conditions) => conditions,
        Err(error) => {
            tracing::error!(%error, "Conditions not fetched");
            return ExitCode::FAILURE;
        }
    };
    let encoded = reference::encode_conditions(&key, &conditions, &provenance)
        .map_err(archive::EncodeRefusal::from)
        .map(|body| (length(&body), archive.create(&object, body)));
    match encoded {
        Ok((bytes, write)) => match write.await {
            Ok(()) => {
                tracing::info!(
                    path = object.path(),
                    codes = conditions.conditions().len(),
                    "Wrote the conditions table"
                );
                match written(journal, object.clone(), bytes) {
                    Ok(()) => ExitCode::SUCCESS,
                    Err(error) => {
                        tracing::error!(%error, "Conditions table write not journaled");
                        ExitCode::FAILURE
                    }
                }
            }
            Err(error) => {
                tracing::error!(%error, "Conditions table not written");
                ExitCode::FAILURE
            }
        },
        Err(refusal) => {
            tracing::error!(%refusal, "Conditions table not encoded");
            ExitCode::FAILURE
        }
    }
}

/// Streams each listed trade file not yet folded from Massive and writes its one-minute, five-minute and daily bars.
async fn fold_trades(
    archive: &Archive,
    flat_files: &FlatFiles,
    alpaca: &Alpaca,
    range: SessionRange,
    concurrency: NonZeroUsize,
    journal: &SharedJournal,
) -> ExitCode {
    let (conditions_key, conditions) = match reference::latest_conditions(archive).await {
        Ok(latest) => latest,
        Err(error) => {
            tracing::error!(%error, "Conditions table not read");
            return ExitCode::FAILURE;
        }
    };
    tracing::info!(
        conditions = Key::from(conditions_key).path(),
        codes = conditions.conditions().len(),
        "Read the conditions table"
    );
    let calendar = match alpaca.calendar(range).await {
        Ok(calendar) => calendar,
        Err(error) => {
            tracing::error!(%error, "Calendar not fetched");
            return ExitCode::FAILURE;
        }
    };
    let daily: TradesKey = massive_derived(BarInterval::OneDay, range.first());
    fold_sessions(
        archive,
        flat_files,
        &calendar,
        FlatFileDataset::Trades,
        daily.into(),
        range,
        concurrency,
        "trade",
        |session, stream| {
            let archive = archive.clone();
            let conditions = conditions.clone();
            let journal = Arc::clone(journal);
            let provenance = provenance(Subscription::StocksAdvanced, Utc::now(), &journal);
            async move {
                fold_trades_one(&archive, stream, session, conditions, &provenance, &journal).await
            }
        },
    )
    .await
}

/// Folds one streamed trade file and creates its three files, the daily last.
async fn fold_trades_one(
    archive: &Archive,
    stream: FlatFileStream,
    session: SessionDate,
    conditions: TradeConditions,
    provenance: &Provenance,
    journal: &Mutex<Journal>,
) -> Result<(), FoldFailure> {
    let folded = tokio::task::spawn_blocking(move || {
        let mut fold = TradeFold::new(session, conditions);
        let mut test_tickers = 0_u64;
        let mut refused = Tally::empty();
        read_trades(stream, |outcome| match outcome {
            TradeRowOutcome::Print {
                print,
                conditions,
                correction,
                ..
            } => fold.push(&print, &conditions, correction),
            TradeRowOutcome::TestTicker => test_tickers += 1,
            TradeRowOutcome::Refused(row) => refused.add(row.cause().kind()),
        })
        .map_err(FoldFailure::Parse)?;
        let (minutes, counts) = fold.finish();
        Ok::<_, FoldFailure>((minutes, counts, test_tickers, refused))
    })
    .await
    .map_err(|error| FoldFailure::Interrupted(error.to_string()))??;
    let (minutes, counts, test_tickers, refused) = folded;
    let minutes = Partition::try_from(minutes).map_err(|refusal| match refusal {
        PartitionRefusal::Empty => FoldFailure::Empty,
    })?;
    let minute_bars = minutes.rows().len();
    let files = session_bars(minutes);
    let symbols = files[2].1.rows().len();
    for (interval, bars) in files {
        let key = massive_derived(interval, session);
        let body = trade_bars::encode(&key, &bars, provenance)
            .map_err(|refusal| FoldFailure::Encode(refusal.into()))?;
        create_or_confirm(archive, &key.into(), body, journal, |held| {
            trade_bars::decode(&key, held).map(|(held, _)| held == bars)
        })
        .await?;
    }
    tracing::info!(
        session = %session,
        symbols,
        minute_bars,
        folded = counts.folded(),
        other_session = counts.other_session(),
        withdrawn = counts.withdrawn(),
        volume_ineligible = counts.volume_ineligible(),
        unsized_prints = counts.unsized_prints(),
        unresolved = counts.unresolved().total(),
        unresolved_by_cause = %counts.unresolved(),
        test_tickers,
        refused = refused.total(),
        refused_by_cause = %refused,
        "Wrote trade bars"
    );
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_a_copy_whose_last_session_precedes_its_first_is_bad_usage() {
        let arguments = |first: &str, last: &str| {
            ["copy", "quotes", first, last, "4"]
                .map(String::from)
                .to_vec()
        };
        assert!(parse(&arguments("2021-08-23", "2021-08-23")).is_some());
        assert!(parse(&arguments("2021-08-24", "2021-08-23")).is_none());
        assert!(
            parse(&["copy", "daily_bars", "2021-08-23", "2021-08-24", "4"].map(String::from))
                .is_none()
        );
        let folding = |first: &str, last: &str, concurrency: &str| {
            parse(&["fold-quotes", first, last, concurrency].map(String::from))
        };
        assert!(folding("2021-08-23", "2021-08-23", "9").is_some());
        assert!(folding("2021-08-24", "2021-08-23", "9").is_none());
        assert!(folding("2021-08-23", "2021-08-24", "0").is_none());
        let day = |text: &str| SessionDate::from_date(text.parse().unwrap());
        for command in ["fold-trades", "fold-quotes"] {
            let ranged = |first: &str, last: &str| {
                parse(&[command, first, last, "2"].map(String::from)).map(|command| match command {
                    Command::FoldTrades { range, .. }
                    | Command::FoldQuotes { range, .. }
                    | Command::Copy { range, .. } => Some((range.first(), range.last())),
                    Command::FetchConditions | Command::Delete { .. } => None,
                })
            };
            assert_eq!(
                ranged("2021-08-23", "2021-08-27"),
                Some(Some((day("2021-08-23"), day("2021-08-27")))),
                "{command}"
            );
            assert_eq!(ranged("2021-08-27", "2021-08-23"), None, "{command}");
        }
    }

    #[test]
    fn test_the_usage_names_every_command_with_its_arguments() {
        assert_eq!(
            usage(),
            concat!(
                "copy <dataset> <first> <last> <concurrency> | fold-quotes <first> <last> <concurrency> | ",
                "fetch-conditions | delete <path>... | fold-trades <first> <last> <concurrency>",
            )
        );
        for name in CommandName::iter() {
            assert_eq!(name.to_string().parse::<CommandName>(), Ok(name));
        }
        assert!(parse(&["fetch-conditions".to_string()]).is_some());
        assert!(parse(&["fetch-conditions", "extra"].map(String::from)).is_none());
        assert!(
            parse(&["fold_quotes", "2021-08-23", "2021-08-24", "2"].map(String::from)).is_none()
        );
    }

    #[test]
    fn test_a_written_object_is_journaled_under_its_key() {
        let directory = std::env::temp_dir().join(format!("fund-raw-{}", uuid::Uuid::new_v4()));
        let journal = Mutex::new(
            Journal::open(
                &directory,
                fund::common::journal::RunId::new(uuid::Uuid::new_v4()),
            )
            .unwrap(),
        );
        let path =
            "data/equity/stage=raw/quotes/provider=massive/year=2021/month=08/day=23/data.csv.gz";
        written(&journal, path.parse::<Key>().unwrap(), 4_096).unwrap();
        let history = journal.into_inner().unwrap().history().unwrap();
        let payloads: Vec<String> = history
            .iter()
            .map(|record| serde_json::to_string(record.observation()).unwrap())
            .collect();
        assert_eq!(
            payloads,
            [format!(
                r#"{{"event_type":"object_written","payload":{{"key":"{path}","bytes":4096}}}}"#
            )]
        );
        std::fs::remove_dir_all(&directory).unwrap();
    }

    #[test]
    fn test_a_delete_names_only_paths_that_parse_as_keys() {
        let key = "data/equity/stage=parsed/trades/provider=massive/interval=one_day/year=2021/month=08/day=23/data.parquet";
        assert!(parse(&["delete".to_string(), key.to_string()]).is_some());
        let outside =
            "data/derived/equity/trades/interval=one_day/year=2021/month=08/day=23/data.parquet";
        assert!(parse(&["delete".to_string(), key.to_string(), outside.to_string()]).is_none());
        assert!(parse(&["delete".to_string()]).is_none());
    }
}
