//! Derivative sums and row counts per input and bin, held as exact integers so they combine in any grouping and a
//! sibling's histogram is its parent's minus the child's.

use crate::common::learning::boosting::bins::{BINS, Code, Codes};
use crate::common::learning::loss::Derivatives;
use crate::common::learning::matrix::Input;
use crate::common::monoid::Monoid;

/// Fixed-point scale for derivatives: 2^32, so a row's gradient is rounded to within 2^-33.
const SCALE: f64 = 4_294_967_296.0;
/// The largest derivative a row may carry, 2^52, so a row's fixed-point value is at most 2^84 and sums of fewer than
/// 2^43 rows stay inside an `i128`.
const LIMIT: f64 = 4_503_599_627_370_496.0;

#[derive(Debug, Clone, Copy, PartialEq, thiserror::Error)]
#[error("a derivative of {value} lies outside the ±2^52 a histogram holds")]
pub struct DerivativeError {
    pub value: f64,
}

/// The summed derivatives and the count of a set of rows.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub struct Statistics {
    gradient: i128,
    hessian: i128,
    count: u64,
}

impl Statistics {
    pub fn of(derivatives: Derivatives) -> Result<Self, DerivativeError> {
        let fixed = |value: f64| match value.abs() <= LIMIT {
            true => Ok((value * SCALE).round() as i128),
            false => Err(DerivativeError { value }),
        };
        Ok(Self {
            gradient: fixed(derivatives.gradient)?,
            hessian: fixed(derivatives.hessian)?,
            count: 1,
        })
    }

    pub fn gradient(&self) -> f64 {
        self.gradient as f64 / SCALE
    }

    pub fn hessian(&self) -> f64 {
        self.hessian as f64 / SCALE
    }

    pub fn count(&self) -> u64 {
        self.count
    }

    /// What remains once `part`, a subset of these rows, is taken out.
    pub fn without(self, part: Self) -> Self {
        Self {
            gradient: self.gradient - part.gradient,
            hessian: self.hessian - part.hessian,
            count: self.count - part.count,
        }
    }

    fn add(&mut self, other: Self) {
        self.gradient += other.gradient;
        self.hessian += other.hessian;
        self.count += other.count;
    }
}

impl Monoid for Statistics {
    fn empty() -> Self {
        Self::default()
    }

    fn combine(mut self, other: Self) -> Self {
        self.add(other);
        self
    }
}

/// `BINS` statistics per input, input-major; the empty histogram holds no slots and stands for no rows.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Histogram(Vec<Statistics>);

impl Histogram {
    /// Folds each listed row's statistics into its bin for every one of `inputs`, leaving the others zero.
    pub fn of(codes: &Codes, statistics: &[Statistics], rows: &[usize], inputs: &[Input]) -> Self {
        let mut slots = vec![Statistics::default(); codes.width() * BINS];
        for &row in rows {
            for &input in inputs {
                slots[slot(input, codes.code(row, input))].add(statistics[row]);
            }
        }
        Self(slots)
    }

    /// The rows of `input` that fell in `code`; none for the empty histogram.
    pub fn slot(&self, input: Input, code: Code) -> Statistics {
        match self.0.is_empty() {
            true => Statistics::default(),
            false => self.0[slot(input, code)],
        }
    }

    /// What remains once `part`, built from a subset of these rows over the same inputs, is taken out.
    pub fn without(self, part: Self) -> Self {
        match (self.0.is_empty(), part.0.is_empty()) {
            (_, true) => self,
            (true, false) => panic!("a part cannot be taken from the empty histogram"),
            (false, false) => {
                assert_eq!(self.0.len(), part.0.len(), "histograms of different widths");
                Self(
                    self.0
                        .into_iter()
                        .zip(part.0)
                        .map(|(whole, part)| whole.without(part))
                        .collect(),
                )
            }
        }
    }
}

fn slot(input: Input, code: Code) -> usize {
    input.index() * BINS + code.index()
}

impl Monoid for Histogram {
    fn empty() -> Self {
        Self(Vec::new())
    }

    /// Both sides must cover the same width unless one is empty.
    fn combine(self, other: Self) -> Self {
        match (self.0.is_empty(), other.0.is_empty()) {
            (true, _) => other,
            (false, true) => self,
            (false, false) => {
                assert_eq!(
                    self.0.len(),
                    other.0.len(),
                    "histograms of different widths"
                );
                Self(
                    self.0
                        .into_iter()
                        .zip(other.0)
                        .map(|(left, right)| left.combine(right))
                        .collect(),
                )
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use std::num::NonZeroUsize;

    use proptest::prelude::*;

    use super::*;
    use crate::common::learning::boosting::bins::Bins;
    use crate::common::learning::matrix::Matrix;
    use crate::common::monoid::{concatenate, laws};

    fn statistics(gradient: f64, hessian: f64) -> Statistics {
        Statistics::of(Derivatives { gradient, hessian }).unwrap()
    }

    fn any_statistics() -> impl Strategy<Value = Statistics> {
        (-1_000_000_i128..1_000_000, 0_i128..1_000_000, 0_u64..1_000).prop_map(
            |(gradient, hessian, count)| Statistics {
                gradient,
                hessian,
                count,
            },
        )
    }

    /// Rows of two inputs over a few distinct values, with each row's derivatives.
    fn any_rows() -> impl Strategy<Value = (Vec<f64>, Vec<Statistics>)> {
        (1_usize..40).prop_flat_map(|rows| {
            (
                prop::collection::vec(prop::option::weighted(0.8, -5_i32..5), rows * 2).prop_map(
                    |values| {
                        values
                            .into_iter()
                            .map(|value| value.map_or(f64::NAN, f64::from))
                            .collect()
                    },
                ),
                prop::collection::vec((-10.0..10.0_f64, 0.0..1.0_f64), rows).prop_map(
                    |derivatives| {
                        derivatives
                            .into_iter()
                            .map(|(gradient, hessian)| statistics(gradient, hessian))
                            .collect()
                    },
                ),
            )
        })
    }

    #[test]
    fn test_statistics_hold_derivatives_in_fixed_point() {
        let sum = statistics(-0.75, 0.25).combine(statistics(0.5, 0.25));
        assert_eq!(
            (sum.gradient(), sum.hessian(), sum.count()),
            (-0.25, 0.5, 2)
        );
    }

    /// A derivative at the limit holds exactly and sums without overflow; one beyond it, or not finite, is refused
    /// rather than clipped.
    #[test]
    fn test_statistics_refuse_a_derivative_beyond_their_exact_range() {
        let limit = 2.0_f64.powi(52);
        let edge = statistics(-limit, limit);
        assert_eq!((edge.gradient(), edge.hessian()), (-limit, limit));
        assert_eq!(concatenate([edge; 4]).gradient(), -4.0 * limit);
        for value in [1e30, -2.0 * limit, f64::NAN, f64::INFINITY] {
            assert_eq!(
                Statistics::of(Derivatives {
                    gradient: value,
                    hessian: 1.0
                })
                .map_err(|error| error.value.to_bits()),
                Err(value.to_bits()),
                "{value}"
            );
            assert!(
                Statistics::of(Derivatives {
                    gradient: 0.0,
                    hessian: value
                })
                .is_err(),
                "{value}"
            );
        }
    }

    #[test]
    #[should_panic(expected = "histograms of different widths")]
    fn test_subtracting_a_histogram_of_another_width_panics() {
        let one = [1.0];
        let two = [1.0, 2.0];
        let histogram = |values: &[f64], width: usize| {
            let matrix = Matrix::new(values, NonZeroUsize::new(width).unwrap()).unwrap();
            let codes = Bins::fit(&matrix).quantize(&matrix);
            Histogram::of(&codes, &[statistics(1.0, 1.0)], &[0], &[Input::new(0)])
        };
        let _ = histogram(&two, 2).without(histogram(&one, 1));
    }

    #[test]
    fn test_a_histogram_bins_each_row_under_each_listed_input_only() {
        let values = [1.0, 10.0, 2.0, f64::NAN];
        let matrix = Matrix::new(&values, NonZeroUsize::new(2).unwrap()).unwrap();
        let bins = Bins::fit(&matrix);
        let codes = bins.quantize(&matrix);
        let rows = [statistics(1.0, 1.0), statistics(-3.0, 1.0)];
        let [first, second] = [0, 1].map(Input::new);
        let histogram = Histogram::of(&codes, &rows, &[0, 1], &[second]);
        assert!(
            bins.codes(first)
                .chain([Code::MISSING])
                .all(|code| histogram.slot(first, code) == Statistics::default())
        );
        assert_eq!(histogram.slot(second, bins.code(second, 10.0)), rows[0]);
        assert_eq!(histogram.slot(second, Code::MISSING), rows[1]);
        assert_eq!(
            Histogram::empty().slot(first, Code::MISSING),
            Statistics::default()
        );
    }

    proptest! {
        #[test]
        fn property_statistics_are_a_commutative_monoid(
            first in any_statistics(),
            second in any_statistics(),
            third in any_statistics(),
        ) {
            laws::check(first, second, third)?;
            prop_assert_eq!(first.combine(second).without(second), first);
        }

        /// A histogram of a row set is the combination of histograms of any split of it, and taking one part out
        /// of the whole leaves exactly the other: the subtraction a tree uses for a sibling.
        #[test]
        fn property_a_histogram_is_a_homomorphism_from_row_sets((values, statistics) in any_rows(), cut in 0.0..1.0_f64) {
            let matrix = Matrix::new(&values, NonZeroUsize::new(2).unwrap()).unwrap();
            let bins = Bins::fit(&matrix);
            let codes = bins.quantize(&matrix);
            let rows: Vec<usize> = (0..statistics.len()).collect();
            let boundary = (cut * rows.len() as f64) as usize;
            let (left, right) = rows.split_at(boundary);
            let inputs = [Input::new(0), Input::new(1)];
            let whole = Histogram::of(&codes, &statistics, &rows, &inputs);
            let parts = [left, right].map(|part| Histogram::of(&codes, &statistics, part, &inputs));
            prop_assert_eq!(&concatenate(parts.clone()), &whole);
            prop_assert_eq!(whole.clone().without(parts[0].clone()), parts[1].clone());
            laws::check(whole.clone(), parts[0].clone(), parts[1].clone())?;
            let total = concatenate(statistics.iter().copied());
            for input in inputs {
                let slots = bins.codes(input).chain([Code::MISSING]).map(|code| whole.slot(input, code));
                prop_assert_eq!(concatenate(slots), total);
            }
        }
    }
}
