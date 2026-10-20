//! Derivative sums and row counts per input and bin, held as exact integers so they combine in any grouping and a
//! sibling's histogram is its parent's minus the child's.

use crate::common::learning::boosting::bins::{BINS, Codes};
use crate::common::learning::loss::Derivatives;
use crate::common::monoid::Monoid;

/// Fixed-point scale for derivatives: 2^32, so a row's gradient is rounded to within 2^-33.
const SCALE: f64 = 4_294_967_296.0;

/// The summed derivatives and the count of a set of rows.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub struct Statistics {
    gradient: i128,
    hessian: i128,
    count: u64,
}

impl Statistics {
    pub fn of(derivatives: Derivatives) -> Self {
        Self {
            gradient: (derivatives.gradient * SCALE).round() as i128,
            hessian: (derivatives.hessian * SCALE).round() as i128,
            count: 1,
        }
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
    pub fn of(codes: &Codes, statistics: &[Statistics], rows: &[usize], inputs: &[usize]) -> Self {
        let mut slots = vec![Statistics::default(); codes.width() * BINS];
        for &row in rows {
            let row_codes = codes.row(row);
            for &input in inputs {
                slots[input * BINS + usize::from(row_codes[input])].add(statistics[row]);
            }
        }
        Self(slots)
    }

    /// One input's slots, indexed by code; empty for the empty histogram.
    pub fn input(&self, input: usize) -> &[Statistics] {
        self.0.get(input * BINS..(input + 1) * BINS).unwrap_or(&[])
    }

    /// What remains once `part`, built from a subset of these rows over the same inputs, is taken out.
    pub fn without(self, part: Self) -> Self {
        match (self.0.is_empty(), part.0.is_empty()) {
            (_, true) => self,
            (true, false) => panic!("a part cannot be taken from the empty histogram"),
            (false, false) => Self(
                self.0
                    .into_iter()
                    .zip(part.0)
                    .map(|(whole, part)| whole.without(part))
                    .collect(),
            ),
        }
    }
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
                            .map(|(gradient, hessian)| {
                                Statistics::of(Derivatives { gradient, hessian })
                            })
                            .collect()
                    },
                ),
            )
        })
    }

    #[test]
    fn test_statistics_hold_derivatives_in_fixed_point() {
        let statistics = Statistics::of(Derivatives {
            gradient: -0.75,
            hessian: 0.25,
        })
        .combine(Statistics::of(Derivatives {
            gradient: 0.5,
            hessian: 0.25,
        }));
        assert_eq!(
            (
                statistics.gradient(),
                statistics.hessian(),
                statistics.count()
            ),
            (-0.25, 0.5, 2)
        );
    }

    #[test]
    fn test_a_histogram_bins_each_row_under_each_listed_input_only() {
        let values = [1.0, 10.0, 2.0, f64::NAN];
        let matrix = Matrix::new(&values, NonZeroUsize::new(2).unwrap()).unwrap();
        let codes = Bins::fit(&matrix).quantize(&matrix);
        let statistics = [
            Statistics::of(Derivatives {
                gradient: 1.0,
                hessian: 1.0,
            }),
            Statistics::of(Derivatives {
                gradient: -3.0,
                hessian: 1.0,
            }),
        ];
        let histogram = Histogram::of(&codes, &statistics, &[0, 1], &[1]);
        assert!(
            histogram
                .input(0)
                .iter()
                .all(|slot| *slot == Statistics::default())
        );
        assert_eq!(histogram.input(1)[0], statistics[0]);
        assert_eq!(histogram.input(1)[255], statistics[1]);
        assert_eq!(Histogram::empty().input(0), []);
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
            let codes = Bins::fit(&matrix).quantize(&matrix);
            let rows: Vec<usize> = (0..statistics.len()).collect();
            let boundary = (cut * rows.len() as f64) as usize;
            let (left, right) = rows.split_at(boundary);
            let inputs = [0, 1];
            let whole = Histogram::of(&codes, &statistics, &rows, &inputs);
            let parts = [left, right].map(|part| Histogram::of(&codes, &statistics, part, &inputs));
            prop_assert_eq!(&concatenate(parts.clone()), &whole);
            prop_assert_eq!(whole.clone().without(parts[0].clone()), parts[1].clone());
            laws::check(whole, parts[0].clone(), parts[1].clone())?;
            let total = concatenate(statistics.iter().copied());
            for input in inputs {
                prop_assert_eq!(concatenate(Histogram::of(&codes, &statistics, &rows, &inputs).input(input).iter().copied()), total);
            }
        }
    }
}
