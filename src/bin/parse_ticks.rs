//! Parses one session's raw ticks from a local copy of the raw object into a local Parquet file, then folds the file's
//! rows back into minute bars and compares them with the archive's:
//! `parse_ticks <massive|alpaca> <trades|quotes> <session> <raw file> <parquet file>`.

use std::collections::BTreeMap;
use std::fs::File;
use std::io::{BufRead, BufReader};
use std::path::{Path, PathBuf};
use std::process::ExitCode;

use arrow_array::RecordBatch;
use chrono::{DateTime, NaiveDate, Utc};
use parquet::arrow::ArrowWriter;
use parquet::arrow::arrow_reader::ParquetRecordBatchReaderBuilder;
use parquet::basic::Compression;
use parquet::file::properties::WriterProperties;
use tracing_subscriber::EnvFilter;

use fund::archive::ticks::{
    QuoteTicks, ROW_GROUP_ROWS, TickRefusal, TradeTicks, quote_schema, trade_schema, trade_ticks,
    two_sided_quotes,
};
use fund::archive::{Archive, quote_bars, reference::latest_conditions, trade_bars};
use fund::common::market::Symbol;
use fund::common::market::quote_bars::QuoteFold;
use fund::common::market::record::BarInterval;
use fund::common::market::trade_bars::{ConditionCode, ConditionLetter, TradeFold};
use fund::common::monoid::Tally;
use fund::common::storage::{Key, Provider, QuotesKey, TradesKey};
use fund::common::time::{SessionDate, SessionRange};
use fund::ingest::RowRefusalKind;
use fund::ingest::alpaca::{
    Alpaca, AlpacaQuoteOutcome, AlpacaTradeOutcome, raw_quote_page, raw_trade_page, tape_of,
};
use fund::ingest::flat_files::{QuoteRowOutcome, TradeRowOutcome, read_quotes, read_trades};
use fund::records::RefusedToStart;

const USAGE: &str =
    "parse_ticks <massive|alpaca> <trades|quotes> <session> <raw file> <parquet file>";

#[derive(Debug, Clone, Copy, strum::Display, strum::EnumString)]
#[strum(serialize_all = "snake_case")]
enum Kind {
    Trades,
    Quotes,
}

/// Why a run failed after it started.
#[derive(Debug, thiserror::Error)]
enum Failure {
    #[error("{0}")]
    Io(#[from] std::io::Error),
    #[error("{0}")]
    Parquet(#[from] parquet::errors::ParquetError),
    #[error("the raw file did not parse: {0}")]
    Raw(String),
    #[error("{0}")]
    Tick(#[from] TickRefusal),
    #[error("the archive's bars did not read: {0}")]
    Archive(String),
    #[error("the session's hours did not read: {0}")]
    Calendar(String),
}

/// What became of the raw file's rows: each is written, a test ticker, or refused with its cause.
#[derive(Debug, Default)]
struct Rows {
    written: u64,
    one_sided: u64,
    test_tickers: u64,
    refused: Tally<RowRefusalKind>,
}

/// The minute bars the ticks fold to against the archive's, keyed by symbol and minute, beside the written ticks
/// that did not read back into a fold.
#[derive(Debug, Default)]
struct Seam {
    unfoldable: u64,
    both: u64,
    ticks_only: u64,
    archive_only: u64,
    differing: u64,
}

impl Seam {
    fn of<Bar: PartialEq>(
        folded: Vec<Bar>,
        archived: &[Bar],
        key: impl Fn(&Bar) -> (Symbol, DateTime<Utc>),
    ) -> Self {
        let mut held: BTreeMap<_, &Bar> = archived.iter().map(|bar| (key(bar), bar)).collect();
        let mut seam = Self::default();
        for bar in folded {
            match held.remove(&key(&bar)) {
                Some(archived) => {
                    seam.both += 1;
                    seam.differing += u64::from(*archived != bar);
                }
                None => seam.ticks_only += 1,
            }
        }
        seam.archive_only = u64::try_from(held.len()).expect("fewer than u64::MAX bars");
        seam
    }

    fn agrees(&self) -> bool {
        self.unfoldable == 0
            && self.ticks_only == 0
            && self.archive_only == 0
            && self.differing == 0
    }
}

fn writer(path: &Path, kind: Kind) -> Result<ArrowWriter<File>, Failure> {
    let schema = match kind {
        Kind::Trades => trade_schema(),
        Kind::Quotes => quote_schema(),
    };
    let properties = WriterProperties::builder()
        .set_compression(Compression::SNAPPY)
        .set_max_row_group_row_count(Some(ROW_GROUP_ROWS))
        .build();
    Ok(ArrowWriter::try_new(
        File::create(path)?,
        schema,
        Some(properties),
    )?)
}

/// Each raw page of an Alpaca tee, one gzipped JSON line per page.
fn pages(raw: &Path) -> Result<impl Iterator<Item = std::io::Result<String>>, Failure> {
    let decoded = flate2::read::MultiGzDecoder::new(BufReader::new(File::open(raw)?));
    Ok(BufReader::new(decoded).lines())
}

fn parse_trades(provider: Provider, raw: &Path, out: &Path) -> Result<Rows, Failure> {
    let mut output = writer(out, Kind::Trades)?;
    let mut ticks = TradeTicks::default();
    let mut rows = Rows::default();
    let mut failure: Option<Failure> = None;
    let flush = |ticks: &mut TradeTicks, output: &mut ArrowWriter<File>| -> Result<(), Failure> {
        if ticks.rows() >= ROW_GROUP_ROWS {
            output.write(&ticks.take())?;
        }
        Ok(())
    };
    match provider {
        Provider::Massive => {
            let read = read_trades(BufReader::new(File::open(raw)?), |outcome| {
                if failure.is_some() {
                    return;
                }
                let pushed = match outcome {
                    TradeRowOutcome::Print {
                        print,
                        conditions,
                        correction,
                        origin,
                    } => ticks
                        .push(
                            &print,
                            conditions.iter().map(|code| code.get().to_string()),
                            correction,
                            &origin,
                        )
                        .map_err(Failure::from)
                        .and_then(|()| flush(&mut ticks, &mut output))
                        .map(|()| rows.written += 1),
                    TradeRowOutcome::TestTicker => {
                        rows.test_tickers += 1;
                        Ok(())
                    }
                    TradeRowOutcome::Refused(row) => {
                        rows.refused.add(row.cause().kind());
                        Ok(())
                    }
                };
                failure = pushed.err();
            });
            if let Some(failure) = failure.take() {
                return Err(failure);
            }
            read.map_err(|refusal| Failure::Raw(refusal.to_string()))?;
        }
        Provider::Alpaca => {
            for page in pages(raw)? {
                let outcomes = raw_trade_page(page?.as_bytes())
                    .map_err(|error| Failure::Raw(error.to_string()))?;
                for outcome in outcomes {
                    match outcome {
                        AlpacaTradeOutcome::Print {
                            print,
                            letters,
                            correction,
                            origin,
                            ..
                        } => {
                            ticks.push(
                                &print,
                                letters.iter().map(|letter| letter.get().to_string()),
                                correction,
                                &origin,
                            )?;
                            flush(&mut ticks, &mut output)?;
                            rows.written += 1;
                        }
                        AlpacaTradeOutcome::Refused(row) => rows.refused.add(row.cause().kind()),
                    }
                }
            }
        }
    }
    output.write(&ticks.take())?;
    output.close()?;
    Ok(rows)
}

fn parse_quotes(provider: Provider, raw: &Path, out: &Path) -> Result<Rows, Failure> {
    let mut output = writer(out, Kind::Quotes)?;
    let mut ticks = QuoteTicks::default();
    let mut rows = Rows::default();
    let mut failure: Option<Failure> = None;
    let flush = |ticks: &mut QuoteTicks, output: &mut ArrowWriter<File>| -> Result<(), Failure> {
        if ticks.rows() >= ROW_GROUP_ROWS {
            output.write(&ticks.take())?;
        }
        Ok(())
    };
    match provider {
        Provider::Massive => {
            let read = read_quotes(BufReader::new(File::open(raw)?), |outcome| {
                if failure.is_some() {
                    return;
                }
                let pushed = match outcome {
                    QuoteRowOutcome::Quote { quote, origin } => ticks
                        .push_quote(&quote, &origin)
                        .map_err(Failure::from)
                        .and_then(|()| flush(&mut ticks, &mut output))
                        .map(|()| rows.written += 1),
                    QuoteRowOutcome::OneSided { quote, origin } => ticks
                        .push_one_sided(&quote, &origin)
                        .map_err(Failure::from)
                        .and_then(|()| flush(&mut ticks, &mut output))
                        .map(|()| {
                            rows.written += 1;
                            rows.one_sided += 1;
                        }),
                    QuoteRowOutcome::TestTicker => {
                        rows.test_tickers += 1;
                        Ok(())
                    }
                    QuoteRowOutcome::Refused(row) => {
                        rows.refused.add(row.cause().kind());
                        Ok(())
                    }
                };
                failure = pushed.err();
            });
            if let Some(failure) = failure.take() {
                return Err(failure);
            }
            read.map_err(|refusal| Failure::Raw(refusal.to_string()))?;
        }
        Provider::Alpaca => {
            for page in pages(raw)? {
                let outcomes = raw_quote_page(page?.as_bytes())
                    .map_err(|error| Failure::Raw(error.to_string()))?;
                for outcome in outcomes {
                    match outcome {
                        AlpacaQuoteOutcome::Quote { quote, origin } => {
                            ticks.push_quote(&quote, &origin)?;
                            flush(&mut ticks, &mut output)?;
                            rows.written += 1;
                        }
                        AlpacaQuoteOutcome::OneSided { quote, origin } => {
                            ticks.push_one_sided(&quote, &origin)?;
                            flush(&mut ticks, &mut output)?;
                            rows.written += 1;
                            rows.one_sided += 1;
                        }
                        AlpacaQuoteOutcome::Refused(row) => rows.refused.add(row.cause().kind()),
                    }
                }
            }
        }
    }
    output.write(&ticks.take())?;
    output.close()?;
    Ok(rows)
}

/// Every batch of the Parquet file at `path`.
fn batches(path: &Path) -> Result<impl Iterator<Item = Result<RecordBatch, Failure>>, Failure> {
    let reader = ParquetRecordBatchReaderBuilder::try_new(File::open(path)?)?.build()?;
    Ok(reader.map(|batch| batch.map_err(|error| Failure::Parquet(error.into()))))
}

/// The written trade ticks folded under the archive's newest conditions table, against its one-minute trade bars;
/// none when the archive holds no bars for the session.
async fn trade_seam(
    archive: &Archive,
    provider: Provider,
    session: SessionDate,
    out: &Path,
) -> Result<Option<Seam>, Failure> {
    let key = TradesKey::new(provider, BarInterval::OneMinute, session);
    let Some(bytes) = archive
        .get(&Key::from(key))
        .await
        .map_err(|error| Failure::Archive(error.to_string()))?
    else {
        return Ok(None);
    };
    let (archived, _) =
        trade_bars::decode(&key, bytes).map_err(|error| Failure::Archive(error.to_string()))?;
    let (_, conditions) = latest_conditions(archive)
        .await
        .map_err(|error| Failure::Archive(error.to_string()))?;
    let mut fold = TradeFold::new(session, conditions);
    let mut unfoldable = 0_u64;
    for batch in batches(out)? {
        for tick in trade_ticks(&batch?)? {
            match provider {
                Provider::Massive => {
                    let codes: Result<Vec<ConditionCode>, _> = tick
                        .conditions
                        .iter()
                        .map(|code| code.parse().map(ConditionCode::new))
                        .collect();
                    match codes {
                        Ok(codes) => fold.push(&tick.print, &codes, tick.correction),
                        Err(_) => unfoldable += 1,
                    }
                }
                Provider::Alpaca => {
                    let letters: Result<Vec<ConditionLetter>, _> = tick
                        .conditions
                        .iter()
                        .map(|letter| ConditionLetter::new(letter))
                        .collect();
                    match (tape_of(&tick.tape), letters) {
                        (Some(tape), Ok(letters)) => {
                            fold.push_lettered(&tick.print, tape, &letters, tick.correction);
                        }
                        (None, _) | (_, Err(_)) => unfoldable += 1,
                    }
                }
            }
        }
    }
    let (folded, _) = fold.finish();
    let seam = Seam::of(folded, archived.rows(), |bar| {
        (bar.symbol().clone(), bar.timestamp())
    });
    Ok(Some(Seam { unfoldable, ..seam }))
}

/// The written two-sided quotes folded over the session's regular hours, against the archive's one-minute quote
/// bars; none when the archive holds no bars for the session.
async fn quote_seam(
    archive: &Archive,
    provider: Provider,
    session: SessionDate,
    out: &Path,
) -> Result<Option<Seam>, Failure> {
    let key = QuotesKey::new(provider, BarInterval::OneMinute, session);
    let Some(bytes) = archive
        .get(&Key::from(key))
        .await
        .map_err(|error| Failure::Archive(error.to_string()))?
    else {
        return Ok(None);
    };
    let (archived, _) =
        quote_bars::decode(&key, bytes).map_err(|error| Failure::Archive(error.to_string()))?;
    let alpaca = Alpaca::from_environment(reqwest::Client::new())
        .map_err(|refusal| Failure::Calendar(refusal.to_string()))?;
    let calendar = alpaca
        .calendar(SessionRange::single(session))
        .await
        .map_err(|error| Failure::Calendar(error.to_string()))?;
    let (open, close) = calendar
        .session(session)
        .map(|trading| trading.hours())
        .ok_or_else(|| Failure::Calendar(format!("{session} is not a trading day")))?;
    let mut fold =
        QuoteFold::new(open, close).map_err(|refusal| Failure::Calendar(refusal.to_string()))?;
    for batch in batches(out)? {
        for quote in two_sided_quotes(&batch?)? {
            fold.push(&quote);
        }
    }
    let (folded, _) = fold.finish();
    Ok(Some(Seam::of(folded, archived.rows(), |bar| {
        (bar.symbol().clone(), bar.timestamp())
    })))
}

async fn run(
    provider: Provider,
    kind: Kind,
    session: SessionDate,
    raw: PathBuf,
    out: PathBuf,
) -> Result<bool, Failure> {
    let parsed = {
        let (raw, out) = (raw.clone(), out.clone());
        tokio::task::spawn_blocking(move || match kind {
            Kind::Trades => parse_trades(provider, &raw, &out),
            Kind::Quotes => parse_quotes(provider, &raw, &out),
        })
        .await
        .expect("the parse does not panic")?
    };
    tracing::info!(%provider, %kind, %session, written = parsed.written, one_sided = parsed.one_sided, test_tickers = parsed.test_tickers, refused = %parsed.refused, "Ticks written");
    let archive = Archive::market_data(&aws_config::load_from_env().await)
        .map_err(|refusal| Failure::Archive(refusal.to_string()))?;
    let seam = match kind {
        Kind::Trades => trade_seam(&archive, provider, session, &out).await?,
        Kind::Quotes => quote_seam(&archive, provider, session, &out).await?,
    };
    match seam {
        Some(seam) => {
            tracing::info!(
                unfoldable = seam.unfoldable,
                both = seam.both,
                ticks_only = seam.ticks_only,
                archive_only = seam.archive_only,
                differing = seam.differing,
                "Seam read"
            );
            Ok(seam.agrees())
        }
        None => {
            tracing::warn!(%provider, %session, "Seam not read, the archive holds no bars for the session");
            Ok(true)
        }
    }
}

#[tokio::main]
async fn main() -> ExitCode {
    tracing_subscriber::fmt()
        .json()
        .with_env_filter(
            EnvFilter::try_from_default_env().unwrap_or_else(|_| EnvFilter::new("info")),
        )
        .init();
    let arguments: Vec<String> = std::env::args().skip(1).collect();
    let parsed = match arguments.as_slice() {
        [provider, kind, session, raw, out] => {
            let provider = provider.parse::<Provider>().ok();
            let kind = kind.parse::<Kind>().ok();
            let session = session
                .parse::<NaiveDate>()
                .ok()
                .map(SessionDate::from_date);
            match (provider, kind, session) {
                (Some(provider), Some(kind), Some(session)) => Some((
                    provider,
                    kind,
                    session,
                    PathBuf::from(raw),
                    PathBuf::from(out),
                )),
                (None, _, _) | (_, None, _) | (_, _, None) => None,
            }
        }
        _ => None,
    };
    let Some((provider, kind, session, raw, out)) = parsed else {
        tracing::error!(?arguments, usage = USAGE, "Usage refused");
        return RefusedToStart.into();
    };
    match run(provider, kind, session, raw, out).await {
        Ok(true) => ExitCode::SUCCESS,
        Ok(false) => {
            tracing::error!("Ticks do not fold to the archive's bars");
            ExitCode::FAILURE
        }
        Err(failure) => {
            tracing::error!(%failure, "Ticks not parsed");
            ExitCode::FAILURE
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn key(bar: &(&'static str, i64, u64)) -> (Symbol, DateTime<Utc>) {
        (
            Symbol::new(bar.0).unwrap(),
            DateTime::from_timestamp(bar.1, 0).unwrap(),
        )
    }

    /// A bar on one side only, or differing, is counted beside the bars both sides hold, and any of them or a tick
    /// that did not fold is disagreement.
    #[test]
    fn test_the_seam_counts_each_side_and_every_difference() {
        let archived = [("ABC", 60, 5), ("ABC", 120, 7), ("XYZ", 60, 1)];
        let folded = vec![("ABC", 60, 5), ("ABC", 120, 8), ("DEF", 60, 2)];
        let seam = Seam::of(folded, &archived, key);
        assert_eq!(
            (
                seam.both,
                seam.ticks_only,
                seam.archive_only,
                seam.differing
            ),
            (2, 1, 1, 1)
        );
        assert!(!seam.agrees());
        assert!(Seam::of(archived.to_vec(), &archived, key).agrees());
        let unfoldable = Seam {
            unfoldable: 1,
            ..Seam::of(archived.to_vec(), &archived, key)
        };
        assert!(!unfoldable.agrees());
    }
}
