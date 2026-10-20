//! A dense row-major matrix of inputs, and the finite labels a fit reads beside it.

use std::num::NonZeroUsize;

#[derive(Debug, Clone, Copy, PartialEq, thiserror::Error)]
pub enum ShapeError {
    #[error("{values} values do not fill whole rows of {width}")]
    Ragged { values: usize, width: usize },
    #[error("{labels} labels for {rows} rows")]
    LabelCount { labels: usize, rows: usize },
    #[error("the label at row {row} is {value}, which is not finite")]
    NonFiniteLabel { row: usize, value: f64 },
}

/// Inputs borrowed row by row; NaN marks a missing value, which every learner routes rather than refuses.
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Matrix<'a> {
    values: &'a [f64],
    width: NonZeroUsize,
}

impl<'a> Matrix<'a> {
    pub fn new(values: &'a [f64], width: NonZeroUsize) -> Result<Self, ShapeError> {
        match values.len() % width.get() {
            0 => Ok(Self { values, width }),
            _ => Err(ShapeError::Ragged {
                values: values.len(),
                width: width.get(),
            }),
        }
    }

    pub fn width(&self) -> usize {
        self.width.get()
    }

    pub fn rows(&self) -> usize {
        self.values.len() / self.width.get()
    }

    pub fn row(&self, row: usize) -> &'a [f64] {
        &self.values[row * self.width.get()..(row + 1) * self.width.get()]
    }
}

/// A matrix with one finite label per row.
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Sample<'a> {
    matrix: Matrix<'a>,
    labels: &'a [f64],
}

impl<'a> Sample<'a> {
    pub fn new(matrix: Matrix<'a>, labels: &'a [f64]) -> Result<Self, ShapeError> {
        if labels.len() != matrix.rows() {
            return Err(ShapeError::LabelCount {
                labels: labels.len(),
                rows: matrix.rows(),
            });
        }
        match labels.iter().position(|label| !label.is_finite()) {
            Some(row) => Err(ShapeError::NonFiniteLabel {
                row,
                value: labels[row],
            }),
            None => Ok(Self { matrix, labels }),
        }
    }

    pub fn matrix(&self) -> Matrix<'a> {
        self.matrix
    }

    pub fn labels(&self) -> &'a [f64] {
        self.labels
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    const TWO: NonZeroUsize = NonZeroUsize::new(2).unwrap();

    #[test]
    fn test_a_matrix_reads_rows_and_refuses_a_ragged_shape() {
        let values = [1.0, 2.0, 3.0, f64::NAN];
        let matrix = Matrix::new(&values, TWO).unwrap();
        assert_eq!((matrix.rows(), matrix.width()), (2, 2));
        assert_eq!(matrix.row(0), [1.0, 2.0]);
        assert!(matrix.row(1)[1].is_nan());
        assert_eq!(
            Matrix::new(&values[..3], TWO),
            Err(ShapeError::Ragged {
                values: 3,
                width: 2
            })
        );
    }

    #[test]
    fn test_a_sample_refuses_a_missing_or_non_finite_label() {
        let values = [1.0, 2.0, 3.0, 4.0];
        let matrix = Matrix::new(&values, TWO).unwrap();
        assert_eq!(
            Sample::new(matrix, &[1.0]),
            Err(ShapeError::LabelCount { labels: 1, rows: 2 })
        );
        assert_eq!(
            Sample::new(matrix, &[1.0, f64::INFINITY]),
            Err(ShapeError::NonFiniteLabel {
                row: 1,
                value: f64::INFINITY
            })
        );
        assert_eq!(
            Sample::new(matrix, &[1.0, 0.0]).unwrap().labels(),
            [1.0, 0.0]
        );
    }
}
