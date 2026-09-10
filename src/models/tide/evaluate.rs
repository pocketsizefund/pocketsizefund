//! Evaluation metrics for a trained TiDE model, on the validation set in scaled space. CRPS here
//! sums pinball loss with a non-strict split where [`crate::models::tide::loss`] averages with a
//! strict one — deliberately different, not a bug to reconcile.

use burn::backend::NdArray;

use crate::models::tide::batch::build_input_tensor;
use crate::models::tide::configuration::ModelParameters;
use crate::models::tide::data::TrainingDataset;
use crate::models::tide::model::TiDEModel;
use crate::models::tide::TideError;

const EVALUATION_BATCH_SIZE: usize = 4096;

/// The trivial forecast every score here is read against: the scaler's own mean at every quantile.
///
/// Zero in scaled space, so it is the constant forecast that has learned the training distribution
/// and nothing else. Named wherever the scores are reported, because a raw CRPS quotes no skill.
pub const BASELINE_NAME: &str = "scaler_mean";

#[derive(Debug, Clone, Copy, serde::Serialize)]
pub struct EvaluationMetrics {
    /// Serialized as `crps`, deliberately, even though the field is spelled out.
    ///
    /// Every artifact already published spells the key that way and the trainer's drift check reads
    /// it back out of them, so renaming it would make the baseline read `None` for every historical
    /// run and suppress drift reporting rather than fail.
    #[serde(rename = "crps")]
    pub continuous_ranked_probability_score: f64,
    /// Measured in scaled space, so it counts the target landing on the same side of the scaler's
    /// mean as the median quantile — not the same side of zero return.
    pub directional_accuracy: f64,
    pub quantile_coverage: f64,
}

impl EvaluationMetrics {
    fn zero() -> Self {
        Self {
            continuous_ranked_probability_score: 0.0,
            directional_accuracy: 0.0,
            quantile_coverage: 0.0,
        }
    }
}

/// What one model scored, beside what the same metrics say about a forecast that knows nothing.
///
/// `baseline.directional_accuracy` is the base rate — the share of targets at or above the scaler
/// mean — which is exactly what a model whose median never turns negative scores, so the model's own
/// figure means nothing read apart from it.
#[derive(Debug, Clone, Copy, serde::Serialize)]
pub struct EvaluationReport {
    pub metrics: EvaluationMetrics,
    pub baseline: EvaluationMetrics,
    pub baseline_name: &'static str,
    /// The span between the outermost quantiles, which is the coverage a calibrated model reaches.
    /// `None` where the artifact emits no quantiles, because then there is no interval to calibrate.
    pub nominal_coverage: Option<f64>,
}

impl EvaluationReport {
    /// A report over a dataset that yielded no rows to score.
    fn empty() -> Self {
        Self {
            metrics: EvaluationMetrics::zero(),
            baseline: EvaluationMetrics::zero(),
            baseline_name: BASELINE_NAME,
            nominal_coverage: None,
        }
    }

    /// How far the model beat [`BASELINE_NAME`]; CRPS is a loss, so positive is the model winning.
    pub fn continuous_ranked_probability_score_skill(&self) -> f64 {
        self.baseline.continuous_ranked_probability_score
            - self.metrics.continuous_ranked_probability_score
    }
}

/// The three positions in the quantile list the metrics read, resolved once per evaluation.
///
/// The quantiles arrive from the artifact in whatever order training wrote them, so "lowest",
/// "highest", and "the median" are positions to be found rather than indices 0, 1, and 2.
#[derive(Debug, Clone, Copy)]
pub(crate) struct QuantileIndices {
    lower: usize,
    pub(crate) median: usize,
    upper: usize,
}

impl QuantileIndices {
    pub(crate) fn locate(quantiles: &[f64]) -> Self {
        Self {
            lower: argmin(quantiles),
            median: closest_to(quantiles, 0.5),
            upper: argmax(quantiles),
        }
    }
}

/// Running totals behind the three metrics.
///
/// Exists so the tests can reach the arithmetic without running the network: one implementation and
/// two callers, so a change to the pinball split, the median selection, or the coverage bounds
/// cannot leave a test green by asserting against its own copy of the loop.
#[derive(Debug, Default, Clone, Copy)]
struct MetricAccumulator {
    pinball_sum: f64,
    directional_matches: usize,
    covered: usize,
    row_count: usize,
}

impl MetricAccumulator {
    /// Adds one horizon step: its predictions ordered to match `quantiles`, and the realized target.
    ///
    /// `row` must be exactly as long as `quantiles`; both callers slice it that way.
    fn add_row(&mut self, row: &[f64], target: f64, quantiles: &[f64], indices: QuantileIndices) {
        for (index, &quantile) in quantiles.iter().enumerate() {
            let error = target - row[index];
            self.pinball_sum += if error >= 0.0 {
                quantile * error
            } else {
                (quantile - 1.0) * error
            };
        }

        // Scaled space: this asks whether the target sits above the scaler's mean, not above zero
        // return, so it is only readable beside the same count for the constant-mean forecast.
        if (row[indices.median] >= 0.0) == (target >= 0.0) {
            self.directional_matches += 1;
        }

        if target >= row[indices.lower] && target <= row[indices.upper] {
            self.covered += 1;
        }

        self.row_count += 1;
    }

    /// Averages the totals over the rows added, or returns zeros when none were.
    fn finish(self) -> EvaluationMetrics {
        if self.row_count == 0 {
            return EvaluationMetrics::zero();
        }
        let rows = self.row_count as f64;
        EvaluationMetrics {
            continuous_ranked_probability_score: self.pinball_sum / rows,
            directional_accuracy: self.directional_matches as f64 / rows,
            quantile_coverage: self.covered as f64 / rows,
        }
    }
}

/// Runs the (inner, non-autodiff) model over the validation dataset and scores it against
/// [`BASELINE_NAME`] over the same rows.
///
/// Returns zeros for an empty or target-less dataset, which is why the trainer refuses an empty
/// validation split before it gets here: zeroed metrics are the best score achievable.
pub fn evaluate(
    model: &TiDEModel<NdArray>,
    dataset: &TrainingDataset,
    parameters: &ModelParameters,
) -> Result<EvaluationReport, TideError> {
    let sample_count = dataset.len();
    let targets = match dataset.targets() {
        Some(targets) if sample_count > 0 => targets,
        _ => return Ok(EvaluationReport::empty()),
    };

    let output_length = parameters.output_length();
    let quantiles = parameters.quantiles();
    let quantile_count = quantiles.len();
    if quantile_count == 0 {
        return Ok(EvaluationReport::empty());
    }

    crate::models::tide::batch::validate_input_shape(dataset, parameters)
        .map_err(TideError::Artifact)?;

    let indices = QuantileIndices::locate(quantiles);

    let device = Default::default();
    let mut predictions: Vec<f32> =
        Vec::with_capacity(sample_count * output_length * quantile_count);
    let sample_indices: Vec<usize> = (0..sample_count).collect();
    for chunk in sample_indices.chunks(EVALUATION_BATCH_SIZE) {
        let input = build_input_tensor::<NdArray>(
            dataset,
            chunk,
            parameters.input_length(),
            output_length,
            &device,
        );
        let output = model.forward(input);
        let mut values: Vec<f32> = output
            .to_data()
            .to_vec()
            .map_err(|error| TideError::Artifact(format!("{error:?}")))?;
        predictions.append(&mut values);
    }

    // The loop below indexes `predictions` arithmetically from the sample, horizon, and quantile
    // counts, so a shorter buffer than those imply is an out-of-bounds panic partway through
    // evaluation rather than an error. That can only happen if the forward pass returned a
    // different shape than the parameters describe -- a real mismatch, but one worth reporting as
    // an error the trainer can log rather than as a crash mid-run.
    let expected_predictions = sample_count * output_length * quantile_count;
    if predictions.len() < expected_predictions {
        return Err(TideError::Artifact(format!(
            "model returned {} prediction values, expected {} for {} samples x {} horizon x {} quantiles",
            predictions.len(),
            expected_predictions,
            sample_count,
            output_length,
            quantile_count
        )));
    }

    let mut accumulator = MetricAccumulator::default();
    let mut baseline_accumulator = MetricAccumulator::default();
    // The trivial forecast, through the same arithmetic: every quantile at the scaler's own mean.
    let baseline_row = vec![0.0; quantile_count];
    // Reused across rows: the model emits `f32` and the metrics are computed in `f64`, so each row
    // is widened once into this buffer rather than allocating one per horizon step.
    let mut row = Vec::with_capacity(quantile_count);

    for sample in 0..sample_count {
        for step in 0..output_length {
            let base = (sample * output_length + step) * quantile_count;
            row.clear();
            row.extend(
                predictions[base..base + quantile_count]
                    .iter()
                    .map(|&prediction| prediction as f64),
            );
            let target = targets[[sample, step, 0]] as f64;
            accumulator.add_row(&row, target, quantiles, indices);
            baseline_accumulator.add_row(&baseline_row, target, quantiles, indices);
        }
    }

    Ok(EvaluationReport {
        metrics: accumulator.finish(),
        baseline: baseline_accumulator.finish(),
        baseline_name: BASELINE_NAME,
        nominal_coverage: Some(quantiles[indices.upper] - quantiles[indices.lower]),
    })
}

fn argmin(values: &[f64]) -> usize {
    values
        .iter()
        .enumerate()
        .min_by(|left, right| left.1.partial_cmp(right.1).unwrap())
        .map(|(index, _)| index)
        .unwrap_or(0)
}

fn argmax(values: &[f64]) -> usize {
    values
        .iter()
        .enumerate()
        .max_by(|left, right| left.1.partial_cmp(right.1).unwrap())
        .map(|(index, _)| index)
        .unwrap_or(0)
}

fn closest_to(values: &[f64], target: f64) -> usize {
    values
        .iter()
        .enumerate()
        .min_by(|left, right| {
            (left.1 - target)
                .abs()
                .partial_cmp(&(right.1 - target).abs())
                .unwrap()
        })
        .map(|(index, _)| index)
        .unwrap_or(0)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::models::tide::configuration::ModelParameters;
    use crate::models::tide::data::TrainingDataset;
    use crate::models::tide::model::TiDEModel;

    /// The serialized key is the contract with every artifact already in the bucket, and nothing
    /// else would notice the `serde(rename)` being dropped: the drift check would read
    /// `metrics.crps`, find nothing, and report `InsufficientHistory` rather than an error.
    #[test]
    fn test_metrics_serialize_under_the_published_crps_key() {
        let metrics = EvaluationMetrics {
            continuous_ranked_probability_score: 0.25,
            directional_accuracy: 0.5,
            quantile_coverage: 0.8,
        };
        let serialized = serde_json::to_value(metrics).expect("metrics must serialize");

        assert_eq!(
            serialized["crps"].as_f64(),
            Some(0.25),
            "the drift check reads `metrics.crps`; got {serialized}"
        );
        assert!(
            serialized
                .get("continuous_ranked_probability_score")
                .is_none(),
            "the spelled-out name must not reach the artifact metadata"
        );
    }

    #[test]
    fn test_argmin_returns_index_of_smallest_value() {
        assert_eq!(argmin(&[0.9, 0.1, 0.5]), 1);
    }

    #[test]
    fn test_argmin_single_element_returns_zero() {
        assert_eq!(argmin(&[1.23]), 0);
    }

    #[test]
    fn test_argmin_empty_returns_zero_fallback() {
        assert_eq!(argmin(&[]), 0);
    }

    #[test]
    fn test_argmax_returns_index_of_largest_value() {
        assert_eq!(argmax(&[0.1, 0.9, 0.5]), 1);
    }

    #[test]
    fn test_argmax_single_element_returns_zero() {
        assert_eq!(argmax(&[2.0]), 0);
    }

    #[test]
    fn test_argmax_empty_returns_zero_fallback() {
        assert_eq!(argmax(&[]), 0);
    }

    #[test]
    fn test_closest_to_returns_nearest_index() {
        // 0.5 is closer to index 1 (0.5) than to 0 (0.1) or 2 (0.9).
        assert_eq!(closest_to(&[0.1, 0.5, 0.9], 0.5), 1);
    }

    #[test]
    fn test_closest_to_breaks_tie_toward_first() {
        // Two values equidistant from target: min_by picks the first encountered.
        assert_eq!(closest_to(&[0.4, 0.6], 0.5), 0);
    }

    #[test]
    fn test_closest_to_single_element_returns_zero() {
        assert_eq!(closest_to(&[0.9], 0.5), 0);
    }

    #[test]
    fn test_closest_to_empty_returns_zero_fallback() {
        assert_eq!(closest_to(&[], 0.5), 0);
    }

    #[test]
    fn test_evaluation_metrics_zero_fields_are_zero() {
        let metrics = EvaluationMetrics::zero();
        assert_eq!(metrics.continuous_ranked_probability_score, 0.0);
        assert_eq!(metrics.directional_accuracy, 0.0);
        assert_eq!(metrics.quantile_coverage, 0.0);
    }

    // metrics_from: feeds a fixed prediction table through the shipped accumulator
    // so the math can be checked without running the neural network.

    /// Runs `predictions` and `targets` through `MetricAccumulator`, the same type `evaluate` uses.
    ///
    /// Reimplements nothing, so every assertion below fails if the pinball split, the median
    /// selection, or the coverage bounds change. The two length assertions matter as much as the
    /// accumulator call: every metric is a mean, and a fixture that zipped short would divide by a
    /// smaller row count and produce a plausible number for a table nobody wrote.
    fn metrics_from(
        predictions: &[[f64; 3]],
        targets: &[f64],
        quantiles: &[f64],
    ) -> EvaluationMetrics {
        assert_eq!(
            predictions.len(),
            targets.len(),
            "fixture must give every prediction row a target"
        );
        assert_eq!(
            quantiles.len(),
            3,
            "rows are [f64; 3], so a shorter list would leave predictions unscored and a longer one \
             would index past the row"
        );

        let indices = QuantileIndices::locate(quantiles);
        let mut accumulator = MetricAccumulator::default();
        for (row, &target) in predictions.iter().zip(targets.iter()) {
            accumulator.add_row(row, target, quantiles, indices);
        }
        accumulator.finish()
    }

    /// The quantile list is whatever training wrote, so the three positions are found rather than
    /// assumed: an artifact listing its quantiles in any other order would otherwise have its
    /// coverage measured between the wrong two bounds.
    #[test]
    fn test_quantile_indices_are_located_not_assumed() {
        let indices = QuantileIndices::locate(&[0.9, 0.1, 0.5]);
        assert_eq!(indices.lower, 1, "0.1 is at position 1");
        assert_eq!(indices.median, 2, "0.5 is at position 2");
        assert_eq!(indices.upper, 0, "0.9 is at position 0");
    }

    /// Coverage is read from the located bounds, so a shuffled quantile list must produce the same
    /// answer as a sorted one over the correspondingly shuffled predictions.
    #[test]
    fn test_coverage_is_unchanged_by_the_order_of_the_quantile_list() {
        let sorted = metrics_from(&[[-0.5, 0.0, 0.5]], &[0.3], &[0.1, 0.5, 0.9]);
        let shuffled = metrics_from(&[[0.5, -0.5, 0.0]], &[0.3], &[0.9, 0.1, 0.5]);

        assert_eq!(sorted.quantile_coverage, 1.0);
        assert_eq!(shuffled.quantile_coverage, sorted.quantile_coverage);
        assert_eq!(shuffled.directional_accuracy, sorted.directional_accuracy);
        assert!(
            (shuffled.continuous_ranked_probability_score
                - sorted.continuous_ranked_probability_score)
                .abs()
                < 1e-9,
            "pinball loss pairs each quantile with its own prediction: {} vs {}",
            shuffled.continuous_ranked_probability_score,
            sorted.continuous_ranked_probability_score
        );
    }

    #[test]
    fn test_continuous_ranked_probability_score_positive_error_branch() {
        // target=1.0, all predictions=0.0; error=1.0 >= 0 for every quantile.
        // row_loss = 0.1*1 + 0.5*1 + 0.9*1 = 1.5; single row so continuous_ranked_probability_score=1.5.
        let metrics = metrics_from(&[[0.0, 0.0, 0.0]], &[1.0], &[0.1, 0.5, 0.9]);
        assert!(
            (metrics.continuous_ranked_probability_score - 1.5).abs() < 1e-9,
            "crps={}",
            metrics.continuous_ranked_probability_score
        );
    }

    #[test]
    fn test_continuous_ranked_probability_score_negative_error_branch() {
        // target=-1.0, all predictions=0.0; error=-1.0 < 0 for every quantile.
        // pinball = (q-1)*error: (0.1-1)*(-1)=0.9, (0.5-1)*(-1)=0.5, (0.9-1)*(-1)=0.1
        // row_loss = 1.5; single row so continuous_ranked_probability_score=1.5.
        let metrics = metrics_from(&[[0.0, 0.0, 0.0]], &[-1.0], &[0.1, 0.5, 0.9]);
        assert!(
            (metrics.continuous_ranked_probability_score - 1.5).abs() < 1e-9,
            "crps={}",
            metrics.continuous_ranked_probability_score
        );
    }

    #[test]
    fn test_continuous_ranked_probability_score_exact_prediction_is_zero() {
        // When every prediction equals the target, error=0 so continuous_ranked_probability_score=0.
        let metrics = metrics_from(&[[0.3, 0.3, 0.3]], &[0.3], &[0.1, 0.5, 0.9]);
        assert!(
            (metrics.continuous_ranked_probability_score).abs() < 1e-9,
            "crps={}",
            metrics.continuous_ranked_probability_score
        );
    }

    #[test]
    fn test_directional_accuracy_both_positive() {
        // q50=0.2>=0 and target=0.3>=0 -> directional match; target within [-0.1,0.5].
        let metrics = metrics_from(&[[-0.1, 0.2, 0.5]], &[0.3], &[0.1, 0.5, 0.9]);
        assert_eq!(metrics.directional_accuracy, 1.0);
        assert_eq!(metrics.quantile_coverage, 1.0);
    }

    #[test]
    fn test_directional_accuracy_mismatch_positive_median_negative_target() {
        // q50 positive but target negative -> no directional match; target outside [0.1,0.5].
        let metrics = metrics_from(&[[0.1, 0.2, 0.5]], &[-0.3], &[0.1, 0.5, 0.9]);
        assert_eq!(metrics.directional_accuracy, 0.0);
        assert_eq!(metrics.quantile_coverage, 0.0);
    }

    #[test]
    fn test_directional_accuracy_both_negative() {
        // q50 < 0 and target < 0 -> directional match.
        let metrics = metrics_from(&[[-0.5, -0.2, -0.1]], &[-0.3], &[0.1, 0.5, 0.9]);
        assert_eq!(metrics.directional_accuracy, 1.0);
    }

    #[test]
    fn test_coverage_target_exactly_at_lower_bound() {
        // target == q_lower (lower bound is inclusive).
        let metrics = metrics_from(&[[-0.3, 0.0, 0.3]], &[-0.3], &[0.1, 0.5, 0.9]);
        assert_eq!(metrics.quantile_coverage, 1.0);
    }

    #[test]
    fn test_coverage_target_exactly_at_upper_bound() {
        // target == q_upper (upper bound is inclusive).
        let metrics = metrics_from(&[[-0.3, 0.0, 0.3]], &[0.3], &[0.1, 0.5, 0.9]);
        assert_eq!(metrics.quantile_coverage, 1.0);
    }

    #[test]
    fn test_multiple_rows_partial_coverage() {
        // Three rows: first two covered, last not.
        let predictions = [[-0.5, 0.0, 0.5], [-0.5, 0.0, 0.5], [-0.5, 0.0, 0.5]];
        let targets = [0.0_f64, 0.3, 1.0];
        let metrics = metrics_from(&predictions, &targets, &[0.1, 0.5, 0.9]);
        let expected_coverage = 2.0 / 3.0;
        assert!(
            (metrics.quantile_coverage - expected_coverage).abs() < 1e-9,
            "coverage={}",
            metrics.quantile_coverage
        );
    }

    /// Construct a minimal dataset with the array shapes expected for
    /// `input_length` and `output_length` so `build_input_tensor` does not
    /// panic, using the tiny 32-input-feature model defined below.
    ///
    /// input_size = input_length*continuous + input_length*categorical + output_length*categorical + static_categorical
    ///            = 2*7 + 2*5 + 1*5 + 3 = 32
    fn make_tiny_dataset(sample_count: usize, with_targets: bool) -> TrainingDataset {
        let input_length = 2_usize;
        let output_length = 1_usize;
        TrainingDataset::new(
            ndarray::Array3::zeros((sample_count, input_length, 7)),
            ndarray::Array3::zeros((sample_count, input_length, 5)),
            ndarray::Array3::zeros((sample_count, output_length, 5)),
            ndarray::Array3::zeros((sample_count, 1, 3)),
            with_targets.then(|| ndarray::Array3::zeros((sample_count, output_length, 1))),
            vec![0; sample_count],
        )
        .unwrap()
    }

    /// `make_tiny_dataset` with the targets set, because every metric here is a mean and a fixture
    /// whose targets never vary makes each one structurally `count / rows` — true of any
    /// implementation, so nothing asserted over it can fail. Inputs stay zero, so the forward pass
    /// returns the same row for every sample and only the targets move.
    fn dataset_with_targets(values: &[f32]) -> TrainingDataset {
        let mut dataset = make_tiny_dataset(values.len(), true);
        let mut targets = ndarray::Array3::<f32>::zeros((values.len(), 1, 1));
        for (sample, value) in values.iter().enumerate() {
            targets[[sample, 0, 0]] = *value;
        }
        dataset = TrainingDataset::new(
            dataset.past_continuous().clone(),
            dataset.past_categorical().clone(),
            dataset.future_categorical().clone(),
            dataset.static_categorical().clone(),
            Some(targets),
            dataset.forecast_sessions().to_vec(),
        )
        .unwrap();
        dataset
    }

    /// Build a TiDEModel that matches `make_tiny_dataset`: input_size=32,
    /// output_length=1, quantile_count=3.
    fn make_tiny_model() -> TiDEModel<NdArray> {
        let device = Default::default();
        TiDEModel::<NdArray>::new(&device, 32, 8, 1, 1, 1, 3, 0.0)
    }

    /// Build ModelParameters aligned with `make_tiny_dataset` and
    /// `make_tiny_model`.
    fn make_tiny_parameters() -> ModelParameters {
        ModelParameters::for_tests(32, 8, 1, 1, 1, 2, 0.0, vec![0.1, 0.5, 0.9], 0.5)
    }

    #[test]
    fn test_evaluate_empty_dataset_returns_zero() {
        let model = make_tiny_model();
        let dataset = make_tiny_dataset(0, true);
        let parameters = make_tiny_parameters();
        let report = evaluate(&model, &dataset, &parameters).unwrap();
        assert_eq!(report.metrics.continuous_ranked_probability_score, 0.0);
        assert_eq!(report.metrics.directional_accuracy, 0.0);
        assert_eq!(report.metrics.quantile_coverage, 0.0);
        assert_eq!(
            report.nominal_coverage, None,
            "no rows scored means no interval was calibrated, which is not a coverage of zero"
        );
    }

    #[test]
    fn test_evaluate_no_targets_returns_zero() {
        // sample_count > 0 but targets is None -> early return zeros.
        let model = make_tiny_model();
        let dataset = make_tiny_dataset(4, false);
        let parameters = make_tiny_parameters();
        let report = evaluate(&model, &dataset, &parameters).unwrap();
        assert_eq!(report.metrics.continuous_ranked_probability_score, 0.0);
        assert_eq!(report.metrics.directional_accuracy, 0.0);
        assert_eq!(report.metrics.quantile_coverage, 0.0);
    }

    #[test]
    fn test_evaluate_empty_quantiles_returns_zero() {
        // quantile_count == 0 triggers the early return after argmin/argmax/closest_to.
        let model = make_tiny_model();
        let dataset = make_tiny_dataset(4, true);
        // Use a model whose quantile list is empty.
        let parameters = ModelParameters::for_tests(32, 8, 1, 1, 1, 2, 0.0, vec![], 0.5);
        let report = evaluate(&model, &dataset, &parameters).unwrap();
        assert_eq!(report.metrics.continuous_ranked_probability_score, 0.0);
        assert_eq!(report.metrics.directional_accuracy, 0.0);
        assert_eq!(report.metrics.quantile_coverage, 0.0);
    }

    /// The trivial forecast is the constant zero, so its CRPS is `1.5 * mean|target|` — 1.5 being
    /// 0.1 + 0.5 + 0.9 — which is zero exactly where the forecast is right and rises with the error.
    ///
    /// Every number below is a literal worked from that identity rather than from the quantile list
    /// the code reads, and each one moves with the fixture, which the all-zeros dataset this replaced
    /// could not: over constant targets every metric is `count / rows` for any implementation at all.
    #[test]
    fn test_the_reported_baseline_is_zero_when_right_and_monotone_in_its_error() {
        let model = make_tiny_model();
        let parameters = make_tiny_parameters();

        let exact = evaluate(&model, &dataset_with_targets(&[0.0, 0.0, 0.0]), &parameters).unwrap();
        assert_eq!(
            exact.baseline.continuous_ranked_probability_score, 0.0,
            "a forecast that is the outcome must cost nothing"
        );

        let near = evaluate(
            &model,
            &dataset_with_targets(&[0.0, 1.0, -1.0]),
            &parameters,
        )
        .unwrap();
        let far = evaluate(
            &model,
            &dataset_with_targets(&[0.0, 2.0, -2.0]),
            &parameters,
        )
        .unwrap();
        assert!(
            (near.baseline.continuous_ranked_probability_score - 1.0).abs() < 1e-9,
            "1.5 * (0 + 1 + 1) / 3, got {}",
            near.baseline.continuous_ranked_probability_score
        );
        assert!(
            (far.baseline.continuous_ranked_probability_score - 2.0).abs() < 1e-9,
            "1.5 * (0 + 2 + 2) / 3, got {}",
            far.baseline.continuous_ranked_probability_score
        );

        assert_eq!(near.baseline_name, "scaler_mean");
        assert!(
            (near.nominal_coverage.unwrap() - 0.8).abs() < 1e-9,
            "the outermost quantiles span 0.9 - 0.1, got {:?}",
            near.nominal_coverage
        );
    }

    /// The base rate is what a median that never turns negative scores, so directional accuracy
    /// cannot be read without it — and it is a fact about the targets, not about the model.
    #[test]
    fn test_the_base_rate_is_reported_beside_directional_accuracy() {
        let model = make_tiny_model();
        let parameters = make_tiny_parameters();

        let report = evaluate(
            &model,
            &dataset_with_targets(&[0.0, 1.0, -1.0]),
            &parameters,
        )
        .unwrap();
        assert!(
            (report.baseline.directional_accuracy - 2.0 / 3.0).abs() < 1e-9,
            "two of the three targets sit at or above the scaler mean, got {}",
            report.baseline.directional_accuracy
        );

        let every_target_below = evaluate(
            &model,
            &dataset_with_targets(&[-1.0, -2.0, -3.0]),
            &parameters,
        )
        .unwrap();
        assert_eq!(
            every_target_below.baseline.directional_accuracy, 0.0,
            "no target above the mean is a base rate of zero, whatever the model scored"
        );
        // Pins the figure to the constant forecast rather than to whatever the network emitted: the
        // model's own accuracy over these targets is 0.0 too whenever its median happens to be
        // non-negative, and only the loss separates the two.
        assert!(
            (every_target_below
                .baseline
                .continuous_ranked_probability_score
                - 3.0)
                .abs()
                < 1e-9,
            "1.5 * (1 + 2 + 3) / 3, got {}",
            every_target_below
                .baseline
                .continuous_ranked_probability_score
        );
    }

    /// The same inputs give the same forward pass, so moving the targets away from it is the only
    /// thing that can change the model's own CRPS — and it must cost more, not less.
    #[test]
    fn test_the_model_score_rises_as_the_targets_move_away_from_it() {
        let model = make_tiny_model();
        let parameters = make_tiny_parameters();

        let near = evaluate(&model, &dataset_with_targets(&[0.0, 0.0, 0.0]), &parameters).unwrap();
        let far = evaluate(
            &model,
            &dataset_with_targets(&[100.0, 100.0, 100.0]),
            &parameters,
        )
        .unwrap();

        assert!(
            far.metrics.continuous_ranked_probability_score
                > near.metrics.continuous_ranked_probability_score,
            "{} against {}",
            far.metrics.continuous_ranked_probability_score,
            near.metrics.continuous_ranked_probability_score
        );
        // Targets all zero, so the trivial forecast is exactly right and nothing can beat it: a
        // skill reported above zero there would mean the difference is signed the wrong way round.
        assert!(
            near.continuous_ranked_probability_score_skill() <= 0.0,
            "skill against a baseline that scored 0.0 was {}",
            near.continuous_ranked_probability_score_skill()
        );
    }

    #[test]
    fn test_evaluate_larger_batch_than_eval_batch_size() {
        // Exercises multi-chunk iteration by exceeding EVALUATION_BATCH_SIZE (4096).
        let model = make_tiny_model();
        let dataset = make_tiny_dataset(4097, true);
        let parameters = make_tiny_parameters();
        let result = evaluate(&model, &dataset, &parameters);
        assert!(
            result.is_ok(),
            "evaluate returned an error: {:?}",
            result.err()
        );
    }
}
