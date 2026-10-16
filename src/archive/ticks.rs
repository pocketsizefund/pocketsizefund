//! Parsed ticks as Parquet: one file per session, provider and kind, written a row group at a time in the raw file's
//! order, with every vendor code kept in the vendor's own spelling.

use std::sync::Arc;

use arrow_array::builder::{
    Decimal128Builder, ListBuilder, StringBuilder, TimestampNanosecondBuilder, UInt64Builder,
};
use arrow_array::{
    Array, ArrayRef, Decimal128Array, ListArray, RecordBatch, StringArray, TimestampNanosecondArray,
};
use arrow_schema::{DataType, Field, Schema, SchemaRef, TimeUnit};
use chrono::{DateTime, Utc};

use crate::common::market::record::Quote;
use crate::common::market::trade_bars::{Correction, Print};
use crate::common::market::{Price, Shares, Symbol};
use crate::ingest::{OneSidedQuote, PricedSide, PrintOrigin, QuoteOrigin};

/// How many rows are buffered before they are written as one row group.
pub const ROW_GROUP_ROWS: usize = 1_000_000;

const PRICE_TYPE: DataType = DataType::Decimal128(18, 6);
const SHARES_TYPE: DataType = DataType::Decimal128(20, 6);

/// Why a tick was not written or not read back.
#[derive(Debug, Clone, PartialEq, Eq, thiserror::Error)]
pub enum TickRefusal {
    #[error("{timestamp} does not fit nanoseconds since the epoch")]
    Unrepresentable { timestamp: DateTime<Utc> },
    #[error("column `{name}` is missing or not of its schema's type")]
    Column { name: &'static str },
    #[error("row {row} does not read back: {reason}")]
    Row { row: usize, reason: String },
}

fn instant(name: &str, nullable: bool) -> Field {
    Field::new(
        name,
        DataType::Timestamp(TimeUnit::Nanosecond, Some("UTC".into())),
        nullable,
    )
}

fn listed(name: &str) -> Field {
    Field::new_list(name, Field::new("item", DataType::Utf8, true), false)
}

pub fn trade_schema() -> SchemaRef {
    Arc::new(Schema::new(vec![
        Field::new("symbol", DataType::Utf8, false),
        instant("timestamp", false),
        Field::new("price", PRICE_TYPE, false),
        Field::new("size", SHARES_TYPE, false),
        Field::new("correction", DataType::Utf8, false),
        Field::new("correction_code", DataType::Utf8, true),
        listed("conditions"),
        Field::new("exchange", DataType::Utf8, true),
        Field::new("tape", DataType::Utf8, false),
        Field::new("id", DataType::Utf8, true),
        instant("participant_timestamp", true),
        instant("trade_reporting_facility_timestamp", true),
        Field::new("sequence", DataType::UInt64, true),
    ]))
}

pub fn quote_schema() -> SchemaRef {
    Arc::new(Schema::new(vec![
        Field::new("symbol", DataType::Utf8, false),
        instant("timestamp", false),
        Field::new("bid_price", PRICE_TYPE, true),
        Field::new("bid_size", SHARES_TYPE, true),
        Field::new("ask_price", PRICE_TYPE, true),
        Field::new("ask_size", SHARES_TYPE, true),
        Field::new("bid_exchange", DataType::Utf8, true),
        Field::new("ask_exchange", DataType::Utf8, true),
        Field::new("tape", DataType::Utf8, true),
        listed("conditions"),
        listed("indicators"),
        instant("participant_timestamp", true),
        Field::new("sequence", DataType::UInt64, true),
    ]))
}

fn nanoseconds(timestamp: DateTime<Utc>) -> Result<i64, TickRefusal> {
    timestamp
        .timestamp_nanos_opt()
        .ok_or(TickRefusal::Unrepresentable { timestamp })
}

fn optional_nanoseconds(timestamp: Option<DateTime<Utc>>) -> Result<Option<i64>, TickRefusal> {
    timestamp.map(nanoseconds).transpose()
}

fn timestamps() -> TimestampNanosecondBuilder {
    TimestampNanosecondBuilder::new().with_timezone("UTC")
}

fn correction_name(correction: Correction) -> &'static str {
    match correction {
        Correction::Stands => "stands",
        Correction::Withdrawn => "withdrawn",
    }
}

/// Trade ticks buffered toward the next row group.
pub struct TradeTicks {
    symbol: StringBuilder,
    timestamp: TimestampNanosecondBuilder,
    price: Decimal128Builder,
    size: Decimal128Builder,
    correction: StringBuilder,
    correction_code: StringBuilder,
    conditions: ListBuilder<StringBuilder>,
    exchange: StringBuilder,
    tape: StringBuilder,
    id: StringBuilder,
    participant_timestamp: TimestampNanosecondBuilder,
    trade_reporting_facility_timestamp: TimestampNanosecondBuilder,
    sequence: UInt64Builder,
    rows: usize,
}

impl Default for TradeTicks {
    fn default() -> Self {
        Self {
            symbol: StringBuilder::new(),
            timestamp: timestamps(),
            price: Decimal128Builder::new(),
            size: Decimal128Builder::new(),
            correction: StringBuilder::new(),
            correction_code: StringBuilder::new(),
            conditions: ListBuilder::new(StringBuilder::new()),
            exchange: StringBuilder::new(),
            tape: StringBuilder::new(),
            id: StringBuilder::new(),
            participant_timestamp: timestamps(),
            trade_reporting_facility_timestamp: timestamps(),
            sequence: UInt64Builder::new(),
            rows: 0,
        }
    }
}

impl TradeTicks {
    /// Buffers one print with the fold's reading of its correction beside the vendor's; an unsized print has size zero.
    pub fn push(
        &mut self,
        print: &Print,
        correction: Correction,
        origin: &PrintOrigin,
    ) -> Result<(), TickRefusal> {
        let timestamp = nanoseconds(print.timestamp())?;
        let participant = optional_nanoseconds(origin.participant_timestamp)?;
        let trade_reporting_facility =
            optional_nanoseconds(origin.trade_reporting_facility_timestamp)?;
        let size = match print {
            Print::Trade(trade) => trade.size().units(),
            Print::Unsized { .. } => 0,
        };
        self.symbol.append_value(print.symbol().as_str());
        self.timestamp.append_value(timestamp);
        self.price.append_value(i128::from(print.price().ticks()));
        self.size.append_value(i128::from(size));
        self.correction.append_value(correction_name(correction));
        self.correction_code
            .append_option(origin.correction.as_deref());
        for condition in &origin.conditions {
            self.conditions.values().append_value(condition);
        }
        self.conditions.append(true);
        self.exchange.append_option(origin.exchange.as_deref());
        self.tape.append_value(&origin.tape);
        self.id.append_option(origin.id.as_deref());
        self.participant_timestamp.append_option(participant);
        self.trade_reporting_facility_timestamp
            .append_option(trade_reporting_facility);
        self.sequence.append_option(origin.sequence);
        self.rows += 1;
        Ok(())
    }

    pub fn rows(&self) -> usize {
        self.rows
    }

    /// The buffered rows as one batch, leaving the buffer empty.
    pub fn take(&mut self) -> RecordBatch {
        self.rows = 0;
        let columns: Vec<ArrayRef> = vec![
            Arc::new(self.symbol.finish()),
            Arc::new(self.timestamp.finish()),
            Arc::new(self.price.finish().with_data_type(PRICE_TYPE)),
            Arc::new(self.size.finish().with_data_type(SHARES_TYPE)),
            Arc::new(self.correction.finish()),
            Arc::new(self.correction_code.finish()),
            Arc::new(self.conditions.finish()),
            Arc::new(self.exchange.finish()),
            Arc::new(self.tape.finish()),
            Arc::new(self.id.finish()),
            Arc::new(self.participant_timestamp.finish()),
            Arc::new(self.trade_reporting_facility_timestamp.finish()),
            Arc::new(self.sequence.finish()),
        ];
        RecordBatch::try_new(trade_schema(), columns).expect("the builders follow the schema")
    }
}

/// Quote ticks, two-sided or not, buffered toward the next row group.
pub struct QuoteTicks {
    symbol: StringBuilder,
    timestamp: TimestampNanosecondBuilder,
    sides: [Decimal128Builder; 4],
    bid_exchange: StringBuilder,
    ask_exchange: StringBuilder,
    tape: StringBuilder,
    conditions: ListBuilder<StringBuilder>,
    indicators: ListBuilder<StringBuilder>,
    participant_timestamp: TimestampNanosecondBuilder,
    sequence: UInt64Builder,
    rows: usize,
}

impl Default for QuoteTicks {
    fn default() -> Self {
        Self {
            symbol: StringBuilder::new(),
            timestamp: timestamps(),
            sides: std::array::from_fn(|_| Decimal128Builder::new()),
            bid_exchange: StringBuilder::new(),
            ask_exchange: StringBuilder::new(),
            tape: StringBuilder::new(),
            conditions: ListBuilder::new(StringBuilder::new()),
            indicators: ListBuilder::new(StringBuilder::new()),
            participant_timestamp: timestamps(),
            sequence: UInt64Builder::new(),
            rows: 0,
        }
    }
}

impl QuoteTicks {
    pub fn push_quote(&mut self, quote: &Quote, origin: &QuoteOrigin) -> Result<(), TickRefusal> {
        self.push(
            quote.symbol(),
            quote.timestamp(),
            Some((quote.bid(), quote.bid_size())),
            Some((quote.ask(), quote.ask_size())),
            origin,
        )
    }

    /// Buffers a quote with a side unpriced, its unpriced sides null.
    pub fn push_one_sided(
        &mut self,
        quote: &OneSidedQuote,
        origin: &QuoteOrigin,
    ) -> Result<(), TickRefusal> {
        let (bid, ask) = match quote.priced {
            PricedSide::Neither => (None, None),
            PricedSide::Bid(price, size) => (Some((price, size)), None),
            PricedSide::Ask(price, size) => (None, Some((price, size))),
        };
        self.push(&quote.symbol, quote.timestamp, bid, ask, origin)
    }

    fn push(
        &mut self,
        symbol: &Symbol,
        timestamp: DateTime<Utc>,
        bid: Option<(Price, Shares)>,
        ask: Option<(Price, Shares)>,
        origin: &QuoteOrigin,
    ) -> Result<(), TickRefusal> {
        let at = nanoseconds(timestamp)?;
        let participant = optional_nanoseconds(origin.participant_timestamp)?;
        self.symbol.append_value(symbol.as_str());
        self.timestamp.append_value(at);
        let [bid_price, bid_size, ask_price, ask_size] = &mut self.sides;
        for (side, price, size) in [(bid, bid_price, bid_size), (ask, ask_price, ask_size)] {
            price.append_option(side.map(|(price, _)| i128::from(price.ticks())));
            size.append_option(side.map(|(_, size)| i128::from(size.units())));
        }
        self.bid_exchange
            .append_option(origin.bid_exchange.as_deref());
        self.ask_exchange
            .append_option(origin.ask_exchange.as_deref());
        self.tape.append_option(origin.tape.as_deref());
        for (builder, elements) in [
            (&mut self.conditions, &origin.conditions),
            (&mut self.indicators, &origin.indicators),
        ] {
            for element in elements {
                builder.values().append_value(element);
            }
            builder.append(true);
        }
        self.participant_timestamp.append_option(participant);
        self.sequence.append_option(origin.sequence);
        self.rows += 1;
        Ok(())
    }

    pub fn rows(&self) -> usize {
        self.rows
    }

    /// The buffered rows as one batch, leaving the buffer empty.
    pub fn take(&mut self) -> RecordBatch {
        self.rows = 0;
        let [bid_price, bid_size, ask_price, ask_size] = &mut self.sides;
        let columns: Vec<ArrayRef> = vec![
            Arc::new(self.symbol.finish()),
            Arc::new(self.timestamp.finish()),
            Arc::new(bid_price.finish().with_data_type(PRICE_TYPE)),
            Arc::new(bid_size.finish().with_data_type(SHARES_TYPE)),
            Arc::new(ask_price.finish().with_data_type(PRICE_TYPE)),
            Arc::new(ask_size.finish().with_data_type(SHARES_TYPE)),
            Arc::new(self.bid_exchange.finish()),
            Arc::new(self.ask_exchange.finish()),
            Arc::new(self.tape.finish()),
            Arc::new(self.conditions.finish()),
            Arc::new(self.indicators.finish()),
            Arc::new(self.participant_timestamp.finish()),
            Arc::new(self.sequence.finish()),
        ];
        RecordBatch::try_new(quote_schema(), columns).expect("the builders follow the schema")
    }
}

/// A trade tick read back with what a fold needs from it.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TradeTick {
    pub print: Print,
    pub conditions: Vec<String>,
    pub correction: Correction,
    pub correction_code: Option<String>,
    pub tape: String,
}

fn column<'a, T: 'static>(
    batch: &'a RecordBatch,
    name: &'static str,
) -> Result<&'a T, TickRefusal> {
    batch
        .column_by_name(name)
        .and_then(|column| column.as_any().downcast_ref::<T>())
        .ok_or(TickRefusal::Column { name })
}

fn row_refusal(row: usize, reason: impl std::fmt::Display) -> TickRefusal {
    TickRefusal::Row {
        row,
        reason: reason.to_string(),
    }
}

fn price_at(prices: &Decimal128Array, row: usize) -> Result<Option<Price>, TickRefusal> {
    match prices.is_null(row) {
        true => Ok(None),
        false => {
            let ticks =
                i64::try_from(prices.value(row)).map_err(|error| row_refusal(row, error))?;
            Price::from_ticks(ticks)
                .map(Some)
                .map_err(|error| row_refusal(row, error))
        }
    }
}

fn shares_at(sizes: &Decimal128Array, row: usize) -> Result<Option<Shares>, TickRefusal> {
    match sizes.is_null(row) {
        true => Ok(None),
        false => u64::try_from(sizes.value(row))
            .map(|units| Some(Shares::from_units(units)))
            .map_err(|error| row_refusal(row, error)),
    }
}

fn symbol_at(symbols: &StringArray, row: usize) -> Result<Symbol, TickRefusal> {
    Symbol::new(symbols.value(row)).map_err(|error| row_refusal(row, error))
}

fn strings_at(lists: &ListArray, row: usize) -> Result<Vec<String>, TickRefusal> {
    let elements = lists.value(row);
    let strings = elements
        .as_any()
        .downcast_ref::<StringArray>()
        .ok_or(TickRefusal::Column { name: "conditions" })?;
    Ok(strings.iter().flatten().map(str::to_string).collect())
}

/// The trade ticks of one batch written by `TradeTicks`, in row order.
pub fn trade_ticks(batch: &RecordBatch) -> Result<Vec<TradeTick>, TickRefusal> {
    let symbols = column::<StringArray>(batch, "symbol")?;
    let timestamps = column::<TimestampNanosecondArray>(batch, "timestamp")?;
    let prices = column::<Decimal128Array>(batch, "price")?;
    let sizes = column::<Decimal128Array>(batch, "size")?;
    let corrections = column::<StringArray>(batch, "correction")?;
    let correction_codes = column::<StringArray>(batch, "correction_code")?;
    let conditions = column::<ListArray>(batch, "conditions")?;
    let tapes = column::<StringArray>(batch, "tape")?;
    (0..batch.num_rows())
        .map(|row| {
            let price = price_at(prices, row)?.ok_or_else(|| row_refusal(row, "no price"))?;
            let size = shares_at(sizes, row)?.ok_or_else(|| row_refusal(row, "no size"))?;
            let correction = match corrections.value(row) {
                "stands" => Correction::Stands,
                "withdrawn" => Correction::Withdrawn,
                other => return Err(row_refusal(row, format!("correction `{other}`"))),
            };
            Ok(TradeTick {
                print: Print::new(
                    symbol_at(symbols, row)?,
                    DateTime::from_timestamp_nanos(timestamps.value(row)),
                    price,
                    size,
                ),
                conditions: strings_at(conditions, row)?,
                correction,
                correction_code: (!correction_codes.is_null(row))
                    .then(|| correction_codes.value(row).to_string()),
                tape: tapes.value(row).to_string(),
            })
        })
        .collect()
}

/// The two-sided quotes of one batch written by `QuoteTicks`, in row order; a row with a side null is no quote.
pub fn two_sided_quotes(batch: &RecordBatch) -> Result<Vec<Quote>, TickRefusal> {
    let symbols = column::<StringArray>(batch, "symbol")?;
    let timestamps = column::<TimestampNanosecondArray>(batch, "timestamp")?;
    let sides = [
        column::<Decimal128Array>(batch, "bid_price")?,
        column::<Decimal128Array>(batch, "bid_size")?,
        column::<Decimal128Array>(batch, "ask_price")?,
        column::<Decimal128Array>(batch, "ask_size")?,
    ];
    let mut quotes = Vec::new();
    for row in 0..batch.num_rows() {
        let [bid_price, bid_size, ask_price, ask_size] = sides;
        let parts = (
            price_at(bid_price, row)?,
            shares_at(bid_size, row)?,
            price_at(ask_price, row)?,
            shares_at(ask_size, row)?,
        );
        let (Some(bid), Some(bid_size), Some(ask), Some(ask_size)) = parts else {
            continue;
        };
        let quote = Quote::new(
            symbol_at(symbols, row)?,
            DateTime::from_timestamp_nanos(timestamps.value(row)),
            bid,
            ask,
            bid_size,
            ask_size,
        )
        .map_err(|error| row_refusal(row, error))?;
        quotes.push(quote);
    }
    Ok(quotes)
}

#[cfg(test)]
mod tests {
    use proptest::prelude::*;

    use super::*;
    use crate::ingest::PricedSide;

    fn at(text: &str) -> DateTime<Utc> {
        text.parse().unwrap()
    }

    fn symbols() -> impl Strategy<Value = Symbol> {
        prop::sample::select(vec!["A", "ABC", "BRK.B", "QQQ"])
            .prop_map(|raw| Symbol::new(raw).unwrap())
    }

    fn instants() -> impl Strategy<Value = DateTime<Utc>> {
        (0_i64..4_102_444_800_000_000_000).prop_map(DateTime::from_timestamp_nanos)
    }

    fn prices() -> impl Strategy<Value = Price> {
        prop_oneof![
            Just(1_i64),
            Just(10_000_000_000_000),
            1_i64..=10_000_000_000_000
        ]
        .prop_map(|ticks| Price::from_ticks(ticks).unwrap())
    }

    fn sizes() -> impl Strategy<Value = Shares> {
        prop_oneof![Just(0_u64), Just(u64::MAX), any::<u64>()].prop_map(Shares::from_units)
    }

    fn codes() -> impl Strategy<Value = String> {
        "[ -~]{0,3}"
    }

    fn origins() -> impl Strategy<Value = PrintOrigin> {
        (
            prop::option::of(codes()),
            codes(),
            prop::option::of(codes()),
            prop::option::of(instants()),
            prop::option::of(instants()),
            prop::option::of(any::<u64>()),
            prop::collection::vec(codes(), 0..4),
            prop::option::of(codes()),
        )
            .prop_map(
                |(exchange, tape, id, participant, facility, sequence, conditions, correction)| {
                    PrintOrigin {
                        exchange,
                        tape,
                        id,
                        participant_timestamp: participant,
                        trade_reporting_facility_timestamp: facility,
                        sequence,
                        conditions,
                        correction,
                    }
                },
            )
    }

    fn quote_origins() -> impl Strategy<Value = QuoteOrigin> {
        (
            prop::option::of(codes()),
            prop::option::of(codes()),
            prop::option::of(codes()),
            prop::collection::vec(codes(), 0..4),
            prop::collection::vec(codes(), 0..4),
            prop::option::of(instants()),
            prop::option::of(any::<u64>()),
        )
            .prop_map(
                |(
                    bid_exchange,
                    ask_exchange,
                    tape,
                    conditions,
                    indicators,
                    participant,
                    sequence,
                )| {
                    QuoteOrigin {
                        bid_exchange,
                        ask_exchange,
                        tape,
                        conditions,
                        indicators,
                        participant_timestamp: participant,
                        sequence,
                    }
                },
            )
    }

    proptest! {
        /// Every print, sized or not, reads back with the fold's correction and the vendor's codes it was written with.
        #[test]
        fn test_trade_ticks_read_back_as_written(
            written in prop::collection::vec(
                (symbols(), instants(), prices(), sizes(), any::<bool>(), origins()),
                0..20,
            )
        ) {
            let mut ticks = TradeTicks::default();
            let mut expected = Vec::new();
            for (symbol, instant, price, size, withdrawn, origin) in written {
                let print = Print::new(symbol, instant, price, size);
                let correction = match withdrawn {
                    true => Correction::Withdrawn,
                    false => Correction::Stands,
                };
                ticks.push(&print, correction, &origin).unwrap();
                expected.push(TradeTick {
                    print,
                    conditions: origin.conditions,
                    correction,
                    correction_code: origin.correction,
                    tape: origin.tape,
                });
            }
            prop_assert_eq!(ticks.rows(), expected.len());
            let batch = ticks.take();
            prop_assert_eq!(ticks.rows(), 0);
            prop_assert_eq!(trade_ticks(&batch).unwrap(), expected);
        }

        /// Every two-sided quote reads back whole, and a one-sided row between them reads as no quote.
        #[test]
        fn test_two_sided_quotes_read_back_as_written(
            written in prop::collection::vec(
                (symbols(), instants(), prices(), 0_i64..1_000_000, sizes(), sizes(), quote_origins()),
                0..20,
            )
        ) {
            let mut ticks = QuoteTicks::default();
            let mut expected = Vec::new();
            for (symbol, instant, bid, spread, bid_size, ask_size, origin) in written {
                let ask = Price::from_ticks((bid.ticks() + spread).min(10_000_000_000_000)).unwrap();
                let quote = Quote::new(
                    symbol.clone(),
                    instant,
                    bid,
                    ask,
                    bid_size,
                    ask_size,
                )
                .unwrap();
                ticks.push_quote(&quote, &origin).unwrap();
                let one_sided = OneSidedQuote {
                    symbol,
                    timestamp: instant,
                    priced: PricedSide::Bid(bid, bid_size),
                };
                ticks.push_one_sided(&one_sided, &origin).unwrap();
                expected.push(quote);
            }
            prop_assert_eq!(ticks.rows(), 2 * expected.len());
            prop_assert_eq!(two_sided_quotes(&ticks.take()).unwrap(), expected);
        }
    }

    /// A two-sided quote reads back whole, and a one-sided one is written with its unpriced side null and read as none.
    #[test]
    fn test_only_two_sided_quotes_read_back() {
        let symbol = Symbol::new("ABC").unwrap();
        let price = |ticks| Price::from_ticks(ticks).unwrap();
        let quote = Quote::new(
            symbol.clone(),
            at("2026-10-09T14:00:00Z"),
            price(50_250_000),
            price(50_270_000),
            Shares::from_units(100_000_000),
            Shares::from_units(200_000_000),
        )
        .unwrap();
        let origin = QuoteOrigin {
            bid_exchange: Some("Q".to_string()),
            ask_exchange: Some(" ".to_string()),
            tape: Some("C".to_string()),
            conditions: vec!["R".to_string()],
            indicators: vec![],
            participant_timestamp: None,
            sequence: None,
        };
        let one_sided = OneSidedQuote {
            symbol,
            timestamp: at("2026-10-09T14:00:01Z"),
            priced: PricedSide::Bid(price(50_250_000), Shares::from_units(100_000_000)),
        };
        let mut ticks = QuoteTicks::default();
        ticks.push_quote(&quote, &origin).unwrap();
        ticks.push_one_sided(&one_sided, &origin).unwrap();
        let batch = ticks.take();
        assert_eq!(batch.num_rows(), 2);
        assert_eq!(
            column::<Decimal128Array>(&batch, "ask_price")
                .unwrap()
                .null_count(),
            1
        );
        assert_eq!(two_sided_quotes(&batch).unwrap(), vec![quote]);
    }
}
