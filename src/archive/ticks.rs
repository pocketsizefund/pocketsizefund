//! Parsed ticks as Parquet: one file per session, provider and kind, written a row group at a time in the raw file's
//! order, with every vendor code kept as the vendor served it.

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
    /// Buffers one print with its conditions spelled as the vendor spells them; an unsized print has size zero.
    pub fn push(
        &mut self,
        print: &Print,
        conditions: impl IntoIterator<Item = String>,
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
        for condition in conditions {
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
    use super::*;

    fn at(text: &str) -> DateTime<Utc> {
        text.parse().unwrap()
    }

    fn origin() -> PrintOrigin {
        PrintOrigin {
            exchange: Some("4".to_string()),
            tape: "1".to_string(),
            id: Some("71675256330320".to_string()),
            participant_timestamp: Some(at("2026-10-09T14:00:00.000000123Z")),
            trade_reporting_facility_timestamp: None,
            sequence: Some(4186),
        }
    }

    /// A trade, an unsized print and a withdrawn print read back as the prints, conditions and corrections written.
    #[test]
    fn test_trade_ticks_read_back_as_written() {
        let symbol = Symbol::new("ABC").unwrap();
        let price = Price::from_ticks(168_890_000).unwrap();
        let written = [
            (
                Print::new(
                    symbol.clone(),
                    at("2026-10-09T14:00:01.5Z"),
                    price,
                    Shares::from_units(49_000_000),
                ),
                vec!["12".to_string(), "37".to_string()],
                Correction::Stands,
            ),
            (
                Print::new(
                    symbol.clone(),
                    at("2026-10-09T20:00:00Z"),
                    price,
                    Shares::from_units(0),
                ),
                vec![],
                Correction::Stands,
            ),
            (
                Print::new(
                    symbol,
                    at("2026-10-09T15:00:00Z"),
                    price,
                    Shares::from_units(2_500),
                ),
                vec!["@".to_string()],
                Correction::Withdrawn,
            ),
        ];
        let mut ticks = TradeTicks::default();
        for (print, conditions, correction) in &written {
            ticks
                .push(print, conditions.clone(), *correction, &origin())
                .unwrap();
        }
        assert_eq!(ticks.rows(), 3);
        let batch = ticks.take();
        assert_eq!(ticks.rows(), 0);
        let read: Vec<_> = trade_ticks(&batch)
            .unwrap()
            .into_iter()
            .map(|tick| (tick.print, tick.conditions, tick.correction, tick.tape))
            .collect();
        let expected: Vec<_> = written
            .into_iter()
            .map(|(print, conditions, correction)| (print, conditions, correction, "1".to_string()))
            .collect();
        assert_eq!(read, expected);
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
