//! Losses as functions of a prediction and a label, with the derivatives every learner descends along.

/// Keeps the logistic base finite when every label agrees.
const LOGISTIC_CLAMP: f64 = 1e-15;

/// A quantile level strictly between zero and one.
#[derive(Debug, Clone, Copy, PartialEq, PartialOrd, serde::Serialize, serde::Deserialize)]
#[serde(try_from = "f64", into = "f64")]
pub struct Level(f64);

#[derive(Debug, Clone, Copy, PartialEq, thiserror::Error)]
#[error("a quantile level lies strictly between 0 and 1, and {0} does not")]
pub struct LevelError(pub f64);

impl TryFrom<f64> for Level {
    type Error = LevelError;

    fn try_from(value: f64) -> Result<Self, Self::Error> {
        if value > 0.0 && value < 1.0 {
            Ok(Self(value))
        } else {
            Err(LevelError(value))
        }
    }
}

impl From<Level> for f64 {
    fn from(level: Level) -> Self {
        level.0
    }
}

/// The first and second derivatives of a loss with respect to the prediction at one row.
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Derivatives {
    pub gradient: f64,
    pub hessian: f64,
}

#[derive(Debug, Clone, Copy, PartialEq, thiserror::Error)]
#[error("logistic loss reads labels of 0 or 1, and the label at row {row} is {value}")]
pub struct LabelError {
    pub row: usize,
    pub value: f64,
}

/// What a fit minimizes; a prediction is on the raw scale, log-odds for `Logistic`, until `response` maps it.
#[derive(Debug, Clone, Copy, PartialEq, serde::Serialize, serde::Deserialize)]
pub enum Loss {
    /// Half the squared error, so the gradient is the residual itself.
    SquaredError,
    /// The pinball loss, whose minimizer is the level's quantile.
    Quantile(Level),
    Logistic,
}

impl Loss {
    pub fn value(self, prediction: f64, label: f64) -> f64 {
        match self {
            Loss::SquaredError => (prediction - label).powi(2) / 2.0,
            Loss::Quantile(Level(level)) => {
                let residual = label - prediction;
                residual * if residual >= 0.0 { level } else { level - 1.0 }
            }
            // ln(1 + e^p) - y p, with the softplus written so neither tail overflows.
            Loss::Logistic => {
                prediction.max(0.0) + (-prediction.abs()).exp().ln_1p() - label * prediction
            }
        }
    }

    /// The pinball loss has no curvature, so its hessian is one by convention and `renewed` sets its leaves.
    pub fn derivatives(self, prediction: f64, label: f64) -> Derivatives {
        match self {
            Loss::SquaredError => Derivatives {
                gradient: prediction - label,
                hessian: 1.0,
            },
            Loss::Quantile(Level(level)) => Derivatives {
                gradient: if label > prediction {
                    -level
                } else {
                    1.0 - level
                },
                hessian: 1.0,
            },
            Loss::Logistic => {
                let probability = sigmoid(prediction);
                Derivatives {
                    gradient: probability - label,
                    hessian: probability * (1.0 - probability),
                }
            }
        }
    }

    /// Maps a raw prediction to the label's scale: a probability for `Logistic`, the identity otherwise.
    pub fn response(self, prediction: f64) -> f64 {
        match self {
            Loss::SquaredError | Loss::Quantile(_) => prediction,
            Loss::Logistic => sigmoid(prediction),
        }
    }

    /// The constant raw prediction that minimizes the loss over `labels`, or zero when there are none.
    pub fn base(self, labels: &[f64]) -> f64 {
        if labels.is_empty() {
            return 0.0;
        }
        let mean = labels.iter().sum::<f64>() / labels.len() as f64;
        match self {
            Loss::SquaredError => mean,
            Loss::Quantile(level) => quantile(&mut labels.to_vec(), level),
            Loss::Logistic => {
                let clamped = mean.clamp(LOGISTIC_CLAMP, 1.0 - LOGISTIC_CLAMP);
                (clamped / (1.0 - clamped)).ln()
            }
        }
    }

    /// A leaf value read straight off the residuals, for a loss whose Newton step says nothing; the order of
    /// `residuals` is not kept.
    pub fn renewed(self, residuals: &mut [f64]) -> Option<f64> {
        match self {
            Loss::SquaredError | Loss::Logistic => None,
            Loss::Quantile(level) => (!residuals.is_empty()).then(|| quantile(residuals, level)),
        }
    }

    pub fn check(self, labels: &[f64]) -> Result<(), LabelError> {
        match self {
            Loss::SquaredError | Loss::Quantile(_) => Ok(()),
            Loss::Logistic => match labels
                .iter()
                .position(|&label| label != 0.0 && label != 1.0)
            {
                Some(row) => Err(LabelError {
                    row,
                    value: labels[row],
                }),
                None => Ok(()),
            },
        }
    }
}

fn sigmoid(value: f64) -> f64 {
    1.0 / (1.0 + (-value).exp())
}

/// Linear interpolation between the order statistics either side of `level` times the last index.
fn quantile(values: &mut [f64], Level(level): Level) -> f64 {
    values.sort_unstable_by(f64::total_cmp);
    let position = level * (values.len() - 1) as f64;
    let (lower, upper) = (position.floor() as usize, position.ceil() as usize);
    values[lower] + (values[upper] - values[lower]) * (position - lower as f64)
}

#[cfg(test)]
mod tests {
    use proptest::prelude::*;

    use super::*;

    fn level(value: f64) -> Level {
        Level::try_from(value).unwrap()
    }

    #[test]
    fn test_a_level_lies_strictly_inside_the_unit_interval_and_reads_back() {
        for value in [0.0, 1.0, -0.5, f64::NAN] {
            assert!(Level::try_from(value).is_err(), "{value}");
        }
        let loss = Loss::Quantile(level(0.1));
        let text = serde_json::to_string(&loss).unwrap();
        assert_eq!(text, r#"{"Quantile":0.1}"#);
        assert_eq!(serde_json::from_str::<Loss>(&text).unwrap(), loss);
        assert!(serde_json::from_str::<Loss>(r#"{"Quantile":1.0}"#).is_err());
    }

    #[test]
    fn test_each_base_is_its_loss_minimizer() {
        assert_eq!(Loss::SquaredError.base(&[1.0, 2.0, 6.0]), 3.0);
        assert!((Loss::Quantile(level(0.1)).base(&[5.0, 1.0, 3.0, 2.0, 4.0]) - 1.4).abs() < 1e-12);
        assert!((Loss::Logistic.base(&[0.0, 0.0, 0.0, 1.0]) - (1.0_f64 / 3.0).ln()).abs() < 1e-12);
        assert!(Loss::Logistic.base(&[1.0, 1.0]).is_finite());
        assert_eq!(Loss::SquaredError.base(&[]), 0.0);
        assert!((Loss::Quantile(level(0.9)).base(&[5.0, 1.0, 3.0, 2.0, 4.0]) - 4.6).abs() < 1e-12);
    }

    #[test]
    fn test_only_the_quantile_loss_renews_its_leaves() {
        assert_eq!(
            Loss::Quantile(level(0.5)).renewed(&mut [3.0, -1.0, 1.0]),
            Some(1.0)
        );
        assert_eq!(Loss::Quantile(level(0.5)).renewed(&mut []), None);
        assert_eq!(Loss::SquaredError.renewed(&mut [3.0]), None);
        assert_eq!(Loss::Logistic.renewed(&mut [3.0]), None);
    }

    #[test]
    fn test_logistic_refuses_a_label_that_is_not_binary() {
        assert_eq!(
            Loss::Logistic.check(&[0.0, 1.0, 0.5]),
            Err(LabelError { row: 2, value: 0.5 })
        );
        assert_eq!(Loss::Logistic.check(&[0.0, 1.0]), Ok(()));
        assert_eq!(Loss::SquaredError.check(&[0.5]), Ok(()));
    }

    #[test]
    fn test_the_logistic_value_holds_in_both_tails() {
        assert!((Loss::Logistic.value(0.0, 1.0) - 2.0_f64.ln()).abs() < 1e-12);
        assert!((Loss::Logistic.value(800.0, 0.0) - 800.0).abs() < 1e-9);
        assert!(Loss::Logistic.value(-800.0, 0.0) < 1e-300);
        assert_eq!(Loss::Logistic.response(0.0), 0.5);
        assert_eq!(
            Loss::Logistic.derivatives(0.0, 1.0),
            Derivatives {
                gradient: -0.5,
                hessian: 0.25
            }
        );
        let Derivatives { gradient, hessian } = Loss::Logistic.derivatives(3.0_f64.ln(), 0.0);
        assert!((gradient - 0.75).abs() < 1e-12 && (hessian - 0.1875).abs() < 1e-12);
    }

    proptest! {
        /// The gradient is the slope of the value, measured by a central difference away from the pinball's kink.
        #[test]
        fn property_each_gradient_is_the_slope_of_its_value(
            prediction in -5.0..5.0_f64,
            label in -5.0..5.0_f64,
            binary in any::<bool>(),
            quantile_level in 0.05..0.95_f64,
        ) {
            prop_assume!((prediction - label).abs() > 1e-3);
            let step = 1e-6;
            let losses = [
                (Loss::SquaredError, label),
                (Loss::Quantile(level(quantile_level)), label),
                (Loss::Logistic, f64::from(u8::from(binary))),
            ];
            for (loss, label) in losses {
                let slope = (loss.value(prediction + step, label) - loss.value(prediction - step, label)) / (2.0 * step);
                let derivatives = loss.derivatives(prediction, label);
                prop_assert!((slope - derivatives.gradient).abs() < 1e-5, "{loss:?}: {slope} against {derivatives:?}");
                prop_assert!(derivatives.hessian >= 0.0);
            }
        }

        #[test]
        fn property_a_probability_stays_inside_the_unit_interval(prediction in -30.0..30.0_f64) {
            let probability = Loss::Logistic.response(prediction);
            prop_assert!(probability > 0.0 && probability < 1.0);
        }
    }
}
