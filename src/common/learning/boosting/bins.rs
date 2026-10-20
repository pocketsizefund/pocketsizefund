//! Each input quantized once into at most 255 ordered bins, with one more bin for a missing value.

use crate::common::learning::matrix::Matrix;

/// The code a missing value takes, above every value bin.
pub const MISSING: u8 = 255;
/// Value bins and the missing bin, the length of one input's slice of a histogram.
pub const BINS: usize = 256;
const VALUE_BINS: usize = 255;

/// Ascending cut points per input: a value takes the first bin whose cut it does not exceed, or the last above all.
#[derive(Debug, Clone, PartialEq)]
pub struct Bins {
    cuts: Vec<Vec<f64>>,
}

impl Bins {
    /// Places cuts midway between distinct values at equal-count quantiles, so a strictly increasing transform of an
    /// input leaves every code unchanged.
    pub fn fit(matrix: &Matrix) -> Self {
        let cuts = (0..matrix.width())
            .map(|input| {
                let mut values: Vec<f64> = (0..matrix.rows())
                    .map(|row| matrix.row(row)[input])
                    .filter(|value| !value.is_nan())
                    .collect();
                values.sort_unstable_by(f64::total_cmp);
                cuts(&values)
            })
            .collect();
        Self { cuts }
    }

    pub fn code(&self, input: usize, value: f64) -> u8 {
        if value.is_nan() {
            MISSING
        } else {
            self.cuts[input].partition_point(|cut| *cut < value) as u8
        }
    }

    /// The number of value bins for `input`, one more than its cuts.
    pub fn count(&self, input: usize) -> usize {
        self.cuts[input].len() + 1
    }

    /// The largest value `code` admits, or `None` for the last value bin, which admits every value.
    pub fn upper(&self, input: usize, code: u8) -> Option<f64> {
        self.cuts[input].get(usize::from(code)).copied()
    }

    pub fn quantize(&self, matrix: &Matrix) -> Codes {
        let codes = (0..matrix.rows())
            .flat_map(|row| {
                matrix
                    .row(row)
                    .iter()
                    .enumerate()
                    .map(|(input, &value)| self.code(input, value))
            })
            .collect();
        Codes {
            codes,
            width: matrix.width(),
        }
    }
}

/// Every value of a matrix replaced by its bin's code, row-major.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Codes {
    codes: Vec<u8>,
    width: usize,
}

impl Codes {
    pub fn width(&self) -> usize {
        self.width
    }

    pub fn row(&self, row: usize) -> &[u8] {
        &self.codes[row * self.width..(row + 1) * self.width]
    }
}

fn cuts(sorted: &[f64]) -> Vec<f64> {
    let mut distinct: Vec<(f64, usize)> = Vec::new();
    for &value in sorted {
        match distinct.last_mut() {
            Some((last, count)) if *last == value => *count += 1,
            Some(_) | None => distinct.push((value, 1)),
        }
    }
    let midpoint = |index: usize| distinct[index].0 / 2.0 + distinct[index + 1].0 / 2.0;
    if distinct.len() <= VALUE_BINS {
        return (0..distinct.len().saturating_sub(1))
            .map(midpoint)
            .collect();
    }
    let mut cuts = Vec::with_capacity(VALUE_BINS - 1);
    let mut seen = 0;
    for (index, (_, count)) in distinct.iter().enumerate().take(distinct.len() - 1) {
        seen += count;
        if seen * VALUE_BINS >= sorted.len() * (cuts.len() + 1) {
            cuts.push(midpoint(index));
            if cuts.len() == VALUE_BINS - 1 {
                break;
            }
        }
    }
    cuts
}

#[cfg(test)]
mod tests {
    use std::num::NonZeroUsize;

    use proptest::prelude::*;

    use super::*;

    fn column(values: &[f64]) -> Matrix<'_> {
        Matrix::new(values, NonZeroUsize::MIN).unwrap()
    }

    #[test]
    fn test_few_distinct_values_each_take_a_bin_and_missing_takes_its_own() {
        let values = [3.0, 1.0, f64::NAN, 2.0, 4.0, 2.0];
        let bins = Bins::fit(&column(&values));
        assert_eq!(bins.cuts, [vec![1.5, 2.5, 3.5]]);
        assert_eq!(bins.count(0), 4);
        assert_eq!(
            bins.quantize(&column(&values)).codes,
            [2, 0, MISSING, 1, 3, 1]
        );
        assert_eq!(
            (bins.code(0, 2.5), bins.code(0, 2.6), bins.code(0, -9.0)),
            (1, 2, 0)
        );
        assert_eq!((bins.upper(0, 1), bins.upper(0, 3)), (Some(2.5), None));
    }

    #[test]
    fn test_an_input_never_observed_has_one_value_bin() {
        let bins = Bins::fit(&column(&[f64::NAN, f64::NAN]));
        assert_eq!(
            (bins.count(0), bins.code(0, 7.0), bins.code(0, f64::NAN)),
            (1, 0, MISSING)
        );
    }

    #[test]
    fn test_many_distinct_values_fill_every_bin_evenly() {
        let values: Vec<f64> = (0..2_550).map(f64::from).collect();
        let bins = Bins::fit(&column(&values));
        assert_eq!(bins.count(0), 255);
        let codes = bins.quantize(&column(&values)).codes;
        let mut sizes = [0_usize; 255];
        for code in codes {
            sizes[usize::from(code)] += 1;
        }
        assert_eq!(
            (sizes.iter().min(), sizes.iter().max()),
            (Some(&10), Some(&10))
        );
    }

    proptest! {
        /// Codes depend only on the order of values, so an exact strictly increasing map changes none of them.
        #[test]
        fn property_an_increasing_transform_keeps_every_code(
            integers in prop::collection::vec(prop::option::weighted(0.9, -400_i32..400), 1..900),
        ) {
            let values: Vec<f64> = integers.iter().map(|value| value.map_or(f64::NAN, f64::from)).collect();
            let transformed: Vec<f64> = values.iter().map(|value| 3.0 * value + 1.0).collect();
            let codes = Bins::fit(&column(&values)).quantize(&column(&values));
            prop_assert_eq!(&codes, &Bins::fit(&column(&transformed)).quantize(&column(&transformed)));
            let bins = Bins::fit(&column(&values));
            prop_assert!(bins.cuts[0].windows(2).all(|pair| pair[0] < pair[1]));
            prop_assert!(codes.codes.iter().all(|&code| code == MISSING || usize::from(code) < bins.count(0)));
        }
    }
}
