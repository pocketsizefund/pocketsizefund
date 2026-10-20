//! Each input quantized once into at most 255 ordered bins, with one more bin for a missing value.

use crate::common::learning::matrix::{Input, Matrix};

/// Value bins and the missing bin, the length of one input's slice of a histogram.
pub const BINS: usize = 256;
const VALUE_BINS: usize = 255;

/// A value's bin within one input, ordered like the values it holds, or the missing bin above them all.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct Code(u8);

impl Code {
    pub const MISSING: Code = Code(255);

    /// The bin's position in its input's slice of a histogram.
    pub fn index(self) -> usize {
        usize::from(self.0)
    }
}

/// Ascending cut points per input: a value takes the first bin whose cut it does not exceed, or the last above all.
#[derive(Debug, Clone, PartialEq)]
pub struct Bins {
    cuts: Vec<Vec<f64>>,
}

impl Bins {
    /// Places cuts between distinct values so each bin holds about an equal share of the rows the bins below it
    /// left, so a strictly increasing transform of an input leaves every code unchanged.
    pub fn fit(matrix: &Matrix) -> Self {
        let cuts = matrix
            .inputs()
            .map(|input| {
                let mut values: Vec<f64> = (0..matrix.rows())
                    .map(|row| matrix.row(row)[input.index()])
                    .filter(|value| !value.is_nan())
                    .collect();
                values.sort_unstable_by(f64::total_cmp);
                cuts(&values)
            })
            .collect();
        Self { cuts }
    }

    pub fn code(&self, input: Input, value: f64) -> Code {
        if value.is_nan() {
            Code::MISSING
        } else {
            Code(self.cuts[input.index()].partition_point(|cut| *cut < value) as u8)
        }
    }

    /// The value bins of `input` in ascending order, one more than its cuts.
    pub fn codes(&self, input: Input) -> impl Iterator<Item = Code> + use<> {
        (0..=self.cuts[input.index()].len() as u8).map(Code)
    }

    /// The largest value `code` admits, or `None` for the last value bin, which admits every value.
    pub fn upper(&self, input: Input, code: Code) -> Option<f64> {
        self.cuts[input.index()].get(code.index()).copied()
    }

    pub fn quantize(&self, matrix: &Matrix) -> Codes {
        let codes = (0..matrix.rows())
            .flat_map(|row| {
                matrix
                    .inputs()
                    .map(move |input| self.code(input, matrix.row(row)[input.index()]))
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
    codes: Vec<Code>,
    width: usize,
}

impl Codes {
    pub fn width(&self) -> usize {
        self.width
    }

    pub fn code(&self, row: usize, input: Input) -> Code {
        self.codes[row * self.width..(row + 1) * self.width][input.index()]
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
    // Halving each side first cannot overflow, but between adjacent doubles the midpoint can round onto the upper.
    let midpoint = |index: usize| {
        let (lower, upper) = (distinct[index].0, distinct[index + 1].0);
        Some(lower / 2.0 + upper / 2.0)
            .filter(|middle| (lower..upper).contains(middle))
            .unwrap_or(lower)
    };
    if distinct.len() <= VALUE_BINS {
        return (0..distinct.len().saturating_sub(1))
            .map(midpoint)
            .collect();
    }
    let mut cuts = Vec::with_capacity(VALUE_BINS - 1);
    let (mut seen, mut start) = (0, 0);
    for (index, (_, count)) in distinct.iter().enumerate().take(distinct.len() - 1) {
        seen += count;
        if (seen - start) * (VALUE_BINS - cuts.len()) >= sorted.len() - start {
            cuts.push(midpoint(index));
            start = seen;
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

    const FIRST: Input = Input::new(0);

    fn column(values: &[f64]) -> Matrix<'_> {
        Matrix::new(values, NonZeroUsize::MIN).unwrap()
    }

    fn codes(bins: &Bins, values: &[f64]) -> Vec<u8> {
        bins.quantize(&column(values))
            .codes
            .iter()
            .map(|code| code.0)
            .collect()
    }

    #[test]
    fn test_few_distinct_values_each_take_a_bin_and_missing_takes_its_own() {
        let values = [3.0, 1.0, f64::NAN, 2.0, 4.0, 2.0];
        let bins = Bins::fit(&column(&values));
        assert_eq!(bins.cuts, [vec![1.5, 2.5, 3.5]]);
        assert_eq!(
            bins.codes(FIRST).collect::<Vec<_>>(),
            [0, 1, 2, 3].map(Code)
        );
        assert_eq!(codes(&bins, &values), [2, 0, 255, 1, 3, 1]);
        assert_eq!(
            (
                bins.code(FIRST, 2.5),
                bins.code(FIRST, 2.6),
                bins.code(FIRST, -9.0)
            ),
            (Code(1), Code(2), Code(0))
        );
        assert_eq!(
            (bins.upper(FIRST, Code(1)), bins.upper(FIRST, Code(3))),
            (Some(2.5), None)
        );
    }

    #[test]
    fn test_an_input_never_observed_has_one_value_bin() {
        let bins = Bins::fit(&column(&[f64::NAN, f64::NAN]));
        assert_eq!(
            (
                bins.codes(FIRST).count(),
                bins.code(FIRST, 7.0),
                bins.code(FIRST, f64::NAN)
            ),
            (1, Code(0), Code::MISSING)
        );
    }

    #[test]
    fn test_many_distinct_values_fill_every_bin_evenly() {
        let values: Vec<f64> = (0..2_550).map(f64::from).collect();
        let bins = Bins::fit(&column(&values));
        assert_eq!(bins.codes(FIRST).count(), 255);
        let mut sizes = [0_usize; 255];
        for code in codes(&bins, &values) {
            sizes[usize::from(code)] += 1;
        }
        assert_eq!(
            (sizes.iter().min(), sizes.iter().max()),
            (Some(&10), Some(&10))
        );
    }

    /// The midpoint of two adjacent doubles rounds onto the upper one, which would put both in the lower bin.
    #[test]
    fn test_adjacent_values_take_separate_bins() {
        let values = [1.000_000_000_000_000_2, 1.000_000_000_000_000_4];
        let bins = Bins::fit(&column(&values));
        assert_eq!(codes(&bins, &values), [0, 1]);
    }

    /// A frequent value fills its own bin, and the rest divide what is left evenly rather than one row apiece.
    #[test]
    fn test_a_frequent_value_leaves_the_rest_evenly_binned() {
        let values: Vec<f64> = std::iter::repeat_n(0.0, 1_000)
            .chain((1..=1_000).map(f64::from))
            .collect();
        let bins = Bins::fit(&column(&values));
        let mut sizes = vec![0_usize; bins.codes(FIRST).count()];
        for code in codes(&bins, &values) {
            sizes[usize::from(code)] += 1;
        }
        assert_eq!(sizes[0], 1_000);
        assert_eq!(
            (
                sizes.len(),
                sizes[1..].iter().min(),
                sizes[1..].iter().max()
            ),
            (255, Some(&3), Some(&4))
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
            let bins = Bins::fit(&column(&values));
            let codes = codes(&bins, &values);
            prop_assert_eq!(&codes, &super::tests::codes(&Bins::fit(&column(&transformed)), &transformed));
            prop_assert!(bins.cuts[0].windows(2).all(|pair| pair[0] < pair[1]));
            let count = bins.codes(FIRST).count();
            prop_assert!(codes.iter().all(|&code| code == 255 || usize::from(code) < count));
        }
    }
}
