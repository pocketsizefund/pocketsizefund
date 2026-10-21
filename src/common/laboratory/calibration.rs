//! Whether a forecast's quantile band holds the share of outcomes its levels promise, overall and by decile of the
//! band's width, since a band can be right on average and wrong at every width.

use crate::common::learning::loss::Level;

/// Width deciles a calibration is read in.
pub const WIDTH_DECILES: usize = 10;

/// The two quantile levels a band forecasts, the lower strictly below the upper.
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Levels {
    lower: Level,
    upper: Level,
}

#[derive(Debug, Clone, Copy, PartialEq, thiserror::Error)]
#[error("a band's lower level lies below its upper, and {lower} does not lie below {upper}")]
pub struct LevelsRefusal {
    pub lower: f64,
    pub upper: f64,
}

impl Levels {
    pub fn new(lower: Level, upper: Level) -> Result<Self, LevelsRefusal> {
        let (low, high) = (f64::from(lower), f64::from(upper));
        match low < high {
            true => Ok(Self { lower, upper }),
            false => Err(LevelsRefusal {
                lower: low,
                upper: high,
            }),
        }
    }

    /// The share of outcomes a calibrated band leaves below its lower forecast.
    pub fn expected_below(self) -> f64 {
        f64::from(self.lower)
    }

    /// The share of outcomes a calibrated band leaves above its upper forecast.
    pub fn expected_above(self) -> f64 {
        1.0 - f64::from(self.upper)
    }
}

/// One forecast band and the outcome that followed; a crossed band, its upper below its lower, is kept and counted.
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Band {
    lower: f64,
    upper: f64,
    outcome: f64,
}

#[derive(Debug, Clone, Copy, PartialEq, thiserror::Error)]
#[error("a band is finite, and lower {lower}, upper {upper} with outcome {outcome} is not")]
pub struct BandRefusal {
    pub lower: f64,
    pub upper: f64,
    pub outcome: f64,
}

impl Band {
    pub fn new(lower: f64, upper: f64, outcome: f64) -> Result<Self, BandRefusal> {
        match lower.is_finite() && upper.is_finite() && outcome.is_finite() {
            true => Ok(Self {
                lower,
                upper,
                outcome,
            }),
            false => Err(BandRefusal {
                lower,
                upper,
                outcome,
            }),
        }
    }

    pub fn width(self) -> f64 {
        self.upper - self.lower
    }
}

/// Counts of bands read, of outcomes below and above them, and of crossed bands; a monoid under `combine`.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Coverage {
    bands: u64,
    below: u64,
    above: u64,
    crossed: u64,
}

impl Coverage {
    pub const EMPTY: Self = Self {
        bands: 0,
        below: 0,
        above: 0,
        crossed: 0,
    };

    fn of(band: Band) -> Self {
        Self {
            bands: 1,
            below: u64::from(band.outcome < band.lower),
            above: u64::from(band.outcome > band.upper),
            crossed: u64::from(band.upper < band.lower),
        }
    }

    pub fn combine(self, other: Self) -> Self {
        Self {
            bands: self.bands + other.bands,
            below: self.below + other.below,
            above: self.above + other.above,
            crossed: self.crossed + other.crossed,
        }
    }

    pub fn bands(self) -> u64 {
        self.bands
    }

    /// The share of outcomes below the lower forecast, `None` over no bands.
    pub fn below(self) -> Option<f64> {
        self.share(self.below)
    }

    pub fn above(self) -> Option<f64> {
        self.share(self.above)
    }

    pub fn crossed(self) -> Option<f64> {
        self.share(self.crossed)
    }

    fn share(self, count: u64) -> Option<f64> {
        (self.bands > 0).then(|| count as f64 / self.bands as f64)
    }
}

/// A band forecast's coverage overall and in each decile of width, narrowest first.
#[derive(Debug, Clone, PartialEq)]
pub struct Calibration {
    levels: Levels,
    overall: Coverage,
    by_width: [Coverage; WIDTH_DECILES],
}

impl Calibration {
    pub fn levels(&self) -> Levels {
        self.levels
    }

    pub fn overall(&self) -> Coverage {
        self.overall
    }

    pub fn by_width(&self) -> &[Coverage; WIDTH_DECILES] {
        &self.by_width
    }
}

/// Reads `bands` overall and in width deciles whose sizes differ by at most one, the larger first; bands of equal
/// width at a decile boundary are split in the order given.
pub fn calibrate(levels: Levels, bands: &[Band]) -> Calibration {
    let mut order: Vec<usize> = (0..bands.len()).collect();
    order.sort_by(|left, right| bands[*left].width().total_cmp(&bands[*right].width()));
    let (size, larger) = (bands.len() / WIDTH_DECILES, bands.len() % WIDTH_DECILES);
    let mut by_width = [Coverage::EMPTY; WIDTH_DECILES];
    let mut start = 0;
    for (decile, coverage) in by_width.iter_mut().enumerate() {
        let end = start + size + usize::from(decile < larger);
        *coverage = order[start..end]
            .iter()
            .map(|index| Coverage::of(bands[*index]))
            .fold(Coverage::EMPTY, Coverage::combine);
        start = end;
    }
    Calibration {
        levels,
        overall: by_width
            .iter()
            .copied()
            .fold(Coverage::EMPTY, Coverage::combine),
        by_width,
    }
}

#[cfg(test)]
mod tests {
    use proptest::prelude::*;

    use super::*;
    use crate::common::laboratory::permutation::Generator;

    fn levels() -> Levels {
        Levels::new(Level::try_from(0.1).unwrap(), Level::try_from(0.9).unwrap()).unwrap()
    }

    #[test]
    fn test_levels_and_bands_refuse_what_cannot_be_read() {
        let half = Level::try_from(0.5).unwrap();
        assert_eq!(
            Levels::new(half, half).map_err(|refusal| refusal.to_string()),
            Err(
                "a band's lower level lies below its upper, and 0.5 does not lie below 0.5"
                    .to_string()
            )
        );
        assert!(Band::new(0.0, f64::NAN, 1.0).is_err());
        assert_eq!(levels().expected_below(), 0.1);
        assert!((levels().expected_above() - 0.1).abs() < 1e-15);
    }

    /// Outcome 5 against [0, 4], [6, 9], [2, 8], the crossed [7, 3] and the empty [5, 5]: a crossed band counts on both sides.
    #[test]
    fn test_coverage_counts_each_side_and_crossed_bands() {
        let bands = [
            Band::new(0.0, 4.0, 5.0).unwrap(),
            Band::new(6.0, 9.0, 5.0).unwrap(),
            Band::new(2.0, 8.0, 5.0).unwrap(),
            Band::new(7.0, 3.0, 5.0).unwrap(),
            Band::new(5.0, 5.0, 5.0).unwrap(),
        ];
        let overall = calibrate(levels(), &bands).overall();
        assert_eq!(
            overall,
            Coverage {
                bands: 5,
                below: 2,
                above: 2,
                crossed: 1
            }
        );
        assert_eq!((overall.below(), overall.crossed()), (Some(0.4), Some(0.2)));
        assert_eq!(Coverage::EMPTY.below(), None);
    }

    /// 23 bands of widths 0 to 22 fill deciles of 3, 3, 3 and then 2, narrowest first, with outcomes above only in
    /// the widest two.
    #[test]
    fn test_deciles_split_by_width_with_the_larger_first() {
        let bands: Vec<Band> = (0..23)
            .rev()
            .map(|width| {
                let outcome = if width >= 21 { 100.0 } else { 0.0 };
                Band::new(0.0, f64::from(width), outcome).unwrap()
            })
            .collect();
        let calibration = calibrate(levels(), &bands);
        let sizes: Vec<u64> = calibration
            .by_width()
            .iter()
            .map(|coverage| coverage.bands())
            .collect();
        assert_eq!(sizes, [3, 3, 3, 2, 2, 2, 2, 2, 2, 2]);
        let above: Vec<u64> = calibration
            .by_width()
            .iter()
            .map(|coverage| coverage.above)
            .collect();
        assert_eq!(above, [0, 0, 0, 0, 0, 0, 0, 0, 0, 2]);
        assert_eq!(calibration.overall().bands(), 23);
    }

    /// Outcomes drawn uniform on [0, 1) against the band [0.1, 0.9] land within four standard errors of the levels
    /// in every decile; against a band shifted up, the machinery reports the miss.
    #[test]
    fn test_a_calibrated_band_reads_its_levels_and_a_shifted_one_does_not() {
        let mut generator = Generator::new(29);
        let outcomes: Vec<f64> = (0..20_000)
            .map(|_| (generator.next_u64() >> 11) as f64 / (1u64 << 53) as f64)
            .collect();
        let read = |lower: f64, upper: f64| {
            let bands: Vec<Band> = outcomes
                .iter()
                .map(|outcome| Band::new(lower, upper, *outcome).unwrap())
                .collect();
            calibrate(levels(), &bands)
        };
        let calibrated = read(0.1, 0.9);
        let standard_error = (0.1 * 0.9 / 2_000.0f64).sqrt();
        for coverage in calibrated.by_width() {
            assert!((coverage.below().unwrap() - 0.1).abs() < 4.0 * standard_error);
            assert!((coverage.above().unwrap() - 0.1).abs() < 4.0 * standard_error);
        }
        let shifted = read(0.3, 1.1).overall();
        assert!((shifted.below().unwrap() - 0.3).abs() < 0.02, "{shifted:?}");
        assert_eq!(shifted.above(), Some(0.0));
    }

    fn coverage() -> impl Strategy<Value = Coverage> {
        (0..1_000u64, 0..1_000u64, 0..1_000u64, 0..1_000u64).prop_map(
            |(bands, below, above, crossed)| Coverage {
                bands,
                below,
                above,
                crossed,
            },
        )
    }

    fn bands() -> impl Strategy<Value = Vec<Band>> {
        prop::collection::vec((-50i32..50, -50i32..50, -50i32..50), 0..80).prop_map(|values| {
            values
                .into_iter()
                .map(|(lower, upper, outcome)| {
                    Band::new(f64::from(lower), f64::from(upper), f64::from(outcome)).unwrap()
                })
                .collect()
        })
    }

    proptest! {
        #[test]
        fn test_coverage_is_a_monoid(first in coverage(), second in coverage(), third in coverage()) {
            prop_assert_eq!(first.combine(Coverage::EMPTY), first);
            prop_assert_eq!(Coverage::EMPTY.combine(first), first);
            prop_assert_eq!(
                first.combine(second).combine(third),
                first.combine(second.combine(third))
            );
        }

        /// Every band lands in exactly one decile, and the deciles differ in size by at most one, the larger first.
        #[test]
        fn test_deciles_partition_the_bands_in_width_order(bands in bands()) {
            let calibration = calibrate(levels(), &bands);
            let sizes: Vec<u64> = calibration.by_width().iter().map(|coverage| coverage.bands()).collect();
            prop_assert_eq!(sizes.iter().sum::<u64>(), bands.len() as u64);
            prop_assert_eq!(calibration.overall().bands(), bands.len() as u64);
            prop_assert!(sizes.iter().max().unwrap() - sizes.iter().min().unwrap() <= 1);
            prop_assert!(sizes.windows(2).all(|pair| pair[0] >= pair[1]));
            let below: u64 = bands.iter().map(|band| u64::from(band.outcome < band.lower)).sum();
            prop_assert_eq!(calibration.overall().below, below);
        }
    }
}
