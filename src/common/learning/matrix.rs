//! A dense row-major matrix of inputs, and the labels a fit reads beside it under one loss.

use std::num::NonZeroUsize;

use crate::common::learning::loss::{Label, LabelError, Loss};

#[derive(Debug, Clone, Copy, PartialEq, thiserror::Error)]
pub enum ShapeError {
    #[error("{values} values do not fill whole rows of {width}")]
    Ragged { values: usize, width: usize },
    #[error("{labels} labels for {rows} rows")]
    LabelCount { labels: usize, rows: usize },
    #[error("the label at row {row} is unreadable")]
    Label {
        row: usize,
        #[source]
        error: LabelError,
    },
}

/// One column of a matrix, by position.
#[derive(
    Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, serde::Serialize, serde::Deserialize,
)]
#[serde(transparent)]
pub struct Input(usize);

impl Input {
    pub const fn new(index: usize) -> Self {
        Self(index)
    }

    pub fn index(self) -> usize {
        self.0
    }
}

/// Inputs borrowed row by row; NaN marks a missing value.
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

    pub fn inputs(&self) -> impl Iterator<Item = Input> + use<> {
        (0..self.width.get()).map(Input)
    }

    pub fn rows(&self) -> usize {
        self.values.len() / self.width.get()
    }

    pub fn row(&self, row: usize) -> &'a [f64] {
        &self.values[row * self.width.get()..(row + 1) * self.width.get()]
    }
}

/// A matrix with one label per row, each read under `loss`.
#[derive(Debug, Clone, PartialEq)]
pub struct Sample<'a> {
    matrix: Matrix<'a>,
    labels: Vec<Label>,
    loss: Loss,
}

impl<'a> Sample<'a> {
    pub fn new(matrix: Matrix<'a>, labels: &[f64], loss: Loss) -> Result<Self, ShapeError> {
        if labels.len() != matrix.rows() {
            return Err(ShapeError::LabelCount {
                labels: labels.len(),
                rows: matrix.rows(),
            });
        }
        let labels = labels
            .iter()
            .enumerate()
            .map(|(row, &value)| {
                loss.label(value)
                    .map_err(|error| ShapeError::Label { row, error })
            })
            .collect::<Result<_, _>>()?;
        Ok(Self {
            matrix,
            labels,
            loss,
        })
    }

    pub fn matrix(&self) -> Matrix<'a> {
        self.matrix
    }

    pub fn labels(&self) -> &[Label] {
        &self.labels
    }

    pub fn loss(&self) -> Loss {
        self.loss
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
        assert_eq!(
            matrix.inputs().collect::<Vec<_>>(),
            [Input::new(0), Input::new(1)]
        );
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
    fn test_a_sample_refuses_a_missing_label_or_one_its_loss_cannot_read() {
        let values = [1.0, 2.0, 3.0, 4.0];
        let matrix = Matrix::new(&values, TWO).unwrap();
        assert_eq!(
            Sample::new(matrix, &[1.0], Loss::SquaredError),
            Err(ShapeError::LabelCount { labels: 1, rows: 2 })
        );
        assert_eq!(
            Sample::new(matrix, &[1.0, f64::INFINITY], Loss::SquaredError),
            Err(ShapeError::Label {
                row: 1,
                error: LabelError {
                    loss: Loss::SquaredError,
                    value: f64::INFINITY
                }
            })
        );
        assert_eq!(
            Sample::new(matrix, &[0.5, 1.0], Loss::Logistic),
            Err(ShapeError::Label {
                row: 0,
                error: LabelError {
                    loss: Loss::Logistic,
                    value: 0.5
                }
            })
        );
        let sample = Sample::new(matrix, &[1.0, 0.0], Loss::Logistic).unwrap();
        assert_eq!(
            sample
                .labels()
                .iter()
                .map(|label| label.get())
                .collect::<Vec<_>>(),
            [1.0, 0.0]
        );
        assert_eq!(sample.loss(), Loss::Logistic);
    }
}
