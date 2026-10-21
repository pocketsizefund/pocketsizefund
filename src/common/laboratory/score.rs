//! A forecast scored by the order it puts names in: one cross-section per instant, averaged within each session into a
//! `Series`, so two models' scores over the same sessions compare through `paired`.

use crate::common::laboratory::series::{Series, SeriesRefusal};
use crate::common::time::SessionDate;

/// Names an instant needs before its score is read.
pub const MINIMUM_NAMES: usize = 20;

/// One name's forecast and the outcome that followed, on scales whose order is all a score reads.
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Pair {
    forecast: f64,
    outcome: f64,
}

#[derive(Debug, Clone, Copy, PartialEq, thiserror::Error)]
#[error("a pair is finite, and forecast {forecast} with outcome {outcome} is not")]
pub struct PairRefusal {
    pub forecast: f64,
    pub outcome: f64,
}

impl Pair {
    pub fn new(forecast: f64, outcome: f64) -> Result<Self, PairRefusal> {
        match forecast.is_finite() && outcome.is_finite() {
            true => Ok(Self { forecast, outcome }),
            false => Err(PairRefusal { forecast, outcome }),
        }
    }

    pub fn forecast(self) -> f64 {
        self.forecast
    }

    pub fn outcome(self) -> f64 {
        self.outcome
    }
}

/// Why an instant has no score, with the count that refused it.
#[derive(Debug, Clone, Copy, PartialEq, Eq, thiserror::Error)]
pub enum Undefined {
    #[error("{names} names is fewer than the {MINIMUM_NAMES} a score needs")]
    TooFewNames { names: usize },
    /// Every forecast was equal, so the instant puts names in no order.
    #[error("all {names} forecasts were equal")]
    ForecastConstant { names: usize },
    #[error("all {names} outcomes were equal")]
    OutcomeConstant { names: usize },
    /// Every pair had a zero forecast or a zero outcome, so no sign could agree.
    #[error("none of {names} names had both a forecast and an outcome away from zero")]
    NoDirection { names: usize },
}

/// What a score measures of one instant's cross-section.
#[derive(
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
    strum::Display,
    strum::EnumString,
    strum::IntoStaticStr,
    strum::EnumIter,
)]
#[strum(serialize_all = "snake_case")]
pub enum Measure {
    /// Spearman's rank correlation, tied values sharing their average rank.
    InformationCoefficient,
    /// The share of names, among those with a nonzero forecast and outcome, whose signs agree.
    HitRate,
}

impl Measure {
    pub fn score(self, pairs: &[Pair]) -> Result<f64, Undefined> {
        let names = pairs.len();
        if names < MINIMUM_NAMES {
            return Err(Undefined::TooFewNames { names });
        }
        match self {
            Self::InformationCoefficient => {
                let forecast = average_ranks(pairs.iter().map(|pair| pair.forecast));
                let outcome = average_ranks(pairs.iter().map(|pair| pair.outcome));
                match (deviations(&forecast), deviations(&outcome)) {
                    (None, _) => Err(Undefined::ForecastConstant { names }),
                    (_, None) => Err(Undefined::OutcomeConstant { names }),
                    (Some(forecast), Some(outcome)) => Ok(correlation(&forecast, &outcome)),
                }
            }
            Self::HitRate => {
                let directional: Vec<bool> = pairs
                    .iter()
                    .filter(|pair| pair.forecast != 0.0 && pair.outcome != 0.0)
                    .map(|pair| (pair.forecast > 0.0) == (pair.outcome > 0.0))
                    .collect();
                match directional.len() {
                    0 => Err(Undefined::NoDirection { names }),
                    total => {
                        let agreed = directional.iter().filter(|agreed| **agreed).count();
                        Ok(agreed as f64 / total as f64)
                    }
                }
            }
        }
    }
}

/// A measure's session means, with how many instants were scored and why each unscored one had no score.
#[derive(Debug, Clone, PartialEq)]
pub struct SessionScores {
    series: Series,
    instants: u64,
    undefined: Vec<(SessionDate, Undefined)>,
}

impl SessionScores {
    /// One reading per session, the mean of its defined instants; a session none of whose instants scored is unmeasured.
    pub fn series(&self) -> &Series {
        &self.series
    }

    pub fn instants(&self) -> u64 {
        self.instants
    }

    pub fn undefined(&self) -> &[(SessionDate, Undefined)] {
        &self.undefined
    }

    /// The share of instants without a score, `None` when no instant was read.
    pub fn undefined_share(&self) -> Option<f64> {
        (self.instants > 0).then(|| self.undefined.len() as f64 / self.instants as f64)
    }
}

/// Scores every instant of every session with `measure` and averages each session's defined scores, so a session
/// weighs the same however many instants it held.
pub fn score_sessions<'a>(
    measure: Measure,
    sessions: impl IntoIterator<Item = (SessionDate, &'a [Vec<Pair>])>,
) -> Result<SessionScores, SeriesRefusal> {
    let (mut instants, mut undefined) = (0, Vec::new());
    let mut readings = Vec::new();
    for (session, cross_sections) in sessions {
        let mut defined = Vec::new();
        for pairs in cross_sections {
            instants += 1;
            match measure.score(pairs) {
                Ok(score) => defined.push(score),
                Err(cause) => undefined.push((session, cause)),
            }
        }
        let mean =
            (!defined.is_empty()).then(|| defined.iter().sum::<f64>() / defined.len() as f64);
        readings.push((session, mean));
    }
    Ok(SessionScores {
        series: Series::new(readings)?,
        instants,
        undefined,
    })
}

/// Ranks from one, smallest first, tied values sharing the mean of the ranks they span.
fn average_ranks(values: impl Iterator<Item = f64>) -> Vec<f64> {
    let values: Vec<f64> = values.collect();
    let mut order: Vec<usize> = (0..values.len()).collect();
    order.sort_by(|left, right| values[*left].total_cmp(&values[*right]));
    let mut ranks = vec![0.0; values.len()];
    let mut start = 0;
    while start < order.len() {
        let end = start
            + order[start..]
                .iter()
                .take_while(|index| values[**index] == values[order[start]])
                .count();
        let shared = (start + end + 1) as f64 / 2.0;
        for index in &order[start..end] {
            ranks[*index] = shared;
        }
        start = end;
    }
    ranks
}

/// Each value less the mean, or `None` where every value is the same.
fn deviations(values: &[f64]) -> Option<Vec<f64>> {
    let mean = values.iter().sum::<f64>() / values.len() as f64;
    let deviations: Vec<f64> = values.iter().map(|value| value - mean).collect();
    deviations
        .iter()
        .any(|deviation| *deviation != 0.0)
        .then_some(deviations)
}

fn correlation(left: &[f64], right: &[f64]) -> f64 {
    let covariance: f64 = left
        .iter()
        .zip(right)
        .map(|(left, right)| left * right)
        .sum();
    let squares = |values: &[f64]| values.iter().map(|value| value * value).sum::<f64>();
    (covariance / (squares(left) * squares(right)).sqrt()).clamp(-1.0, 1.0)
}

#[cfg(test)]
mod tests {
    use chrono::NaiveDate;
    use proptest::prelude::*;

    use super::*;
    use crate::common::laboratory::estimate::{Control, Treatment, paired};
    use crate::common::laboratory::permutation::Generator;

    fn session(day: i64) -> SessionDate {
        SessionDate::from_date(NaiveDate::from_ymd_opt(2026, 3, 2).unwrap()).plus_calendar_days(day)
    }

    fn pairs(forecasts: &[f64], outcomes: &[f64]) -> Vec<Pair> {
        forecasts
            .iter()
            .zip(outcomes)
            .map(|(forecast, outcome)| Pair::new(*forecast, *outcome).unwrap())
            .collect()
    }

    fn line(names: usize) -> Vec<f64> {
        (0..names).map(|name| name as f64).collect()
    }

    #[test]
    fn test_a_pair_refuses_what_cannot_be_ranked() {
        assert_eq!(
            Pair::new(f64::NAN, 1.0).map_err(|refusal| refusal.to_string()),
            Err("a pair is finite, and forecast NaN with outcome 1 is not".to_string())
        );
        assert!(Pair::new(1.0, f64::INFINITY).is_err());
    }

    #[test]
    fn test_ranks_share_their_ties() {
        assert_eq!(
            average_ranks([3.0, 1.0, 3.0, 2.0, 3.0].into_iter()),
            [4.0, 1.0, 4.0, 2.0, 4.0]
        );
        assert_eq!(average_ranks([-0.0, 0.0].into_iter()), [1.5, 1.5]);
    }

    #[test]
    fn test_order_scores_one_and_its_reverse_minus_one() {
        let ascending = line(20);
        let descending: Vec<f64> = ascending.iter().rev().copied().collect();
        let coefficient = Measure::InformationCoefficient;
        assert_eq!(coefficient.score(&pairs(&ascending, &ascending)), Ok(1.0));
        assert_eq!(coefficient.score(&pairs(&ascending, &descending)), Ok(-1.0));
        let centered: Vec<f64> = ascending.iter().map(|value| value - 9.5).collect();
        let negated: Vec<f64> = centered.iter().map(|value| -value).collect();
        assert_eq!(
            Measure::HitRate.score(&pairs(&centered, &centered)),
            Ok(1.0)
        );
        assert_eq!(Measure::HitRate.score(&pairs(&centered, &negated)), Ok(0.0));
    }

    /// Spearman over 20 names with one swapped pair: 1 - 6 * 2 / (20 * 399).
    #[test]
    fn test_a_swap_costs_what_spearman_says() {
        let forecast = line(20);
        let mut outcome = line(20);
        outcome.swap(0, 1);
        let score = Measure::InformationCoefficient
            .score(&pairs(&forecast, &outcome))
            .unwrap();
        assert!((score - (1.0 - 12.0 / 7980.0)).abs() < 1e-12, "{score}");
    }

    #[test]
    fn test_an_instant_without_an_order_is_undefined_not_zero() {
        let constant = vec![0.5; 20];
        let coefficient = Measure::InformationCoefficient;
        assert_eq!(
            coefficient.score(&pairs(&line(19), &line(19))),
            Err(Undefined::TooFewNames { names: 19 })
        );
        assert_eq!(
            coefficient.score(&pairs(&constant, &line(20))),
            Err(Undefined::ForecastConstant { names: 20 })
        );
        assert_eq!(
            coefficient.score(&pairs(&line(20), &constant)),
            Err(Undefined::OutcomeConstant { names: 20 })
        );
        assert_eq!(
            Measure::HitRate.score(&pairs(&[0.0; 20], &line(20))),
            Err(Undefined::NoDirection { names: 20 })
        );
    }

    /// Names with a zero on either side have no direction and leave the denominator: 3 agree of the 4 directional.
    #[test]
    fn test_hit_rate_counts_only_directional_names() {
        let mut forecast = vec![0.0; 20];
        let mut outcome = vec![1.0; 20];
        forecast[..5].copy_from_slice(&[1.0, 2.0, -1.0, -3.0, 4.0]);
        outcome[..5].copy_from_slice(&[1.0, 5.0, -2.0, 1.0, 0.0]);
        assert_eq!(
            Measure::HitRate.score(&pairs(&forecast, &outcome)),
            Ok(0.75)
        );
    }

    #[test]
    fn test_measures_round_trip_through_their_names() {
        assert_eq!(<Measure as strum::IntoEnumIterator>::iter().count(), 2);
        for (measure, name) in [
            (Measure::InformationCoefficient, "information_coefficient"),
            (Measure::HitRate, "hit_rate"),
        ] {
            assert_eq!(measure.to_string(), name);
            assert_eq!(name.parse::<Measure>(), Ok(measure));
        }
    }

    /// Session one averages its two defined instants and counts the third; session two has no defined instant.
    #[test]
    fn test_sessions_average_their_defined_instants_and_count_the_rest() {
        let ascending = line(20);
        let descending: Vec<f64> = ascending.iter().rev().copied().collect();
        let first = [
            pairs(&ascending, &ascending),
            pairs(&ascending, &ascending),
            pairs(&line(3), &line(3)),
        ];
        let second = [pairs(&[1.0; 20], &descending)];
        let scores = score_sessions(
            Measure::InformationCoefficient,
            [(session(0), &first[..]), (session(1), &second[..])],
        )
        .unwrap();
        assert_eq!(
            scores.series(),
            &Series::new([(session(0), Some(1.0)), (session(1), None)]).unwrap()
        );
        assert_eq!(scores.instants(), 4);
        assert_eq!(
            scores.undefined(),
            [
                (session(0), Undefined::TooFewNames { names: 3 }),
                (session(1), Undefined::ForecastConstant { names: 20 }),
            ]
        );
        assert_eq!(scores.undefined_share(), Some(0.5));
        let empty = score_sessions(Measure::HitRate, std::iter::empty()).unwrap();
        assert_eq!(empty.undefined_share(), None);
    }

    /// Two models scored over the same sessions compare through `paired`, as replayed strategies do.
    #[test]
    fn test_two_models_compare_through_paired() {
        let ascending = line(20);
        let mut generator = Generator::new(3);
        let (mut perfect, mut noise) = (Vec::new(), Vec::new());
        for _ in 0..30 {
            let mut shuffled = ascending.clone();
            generator.shuffle(&mut shuffled);
            perfect.push(vec![pairs(&ascending, &ascending)]);
            noise.push(vec![pairs(&shuffled, &ascending)]);
        }
        let score = |model: &[Vec<Vec<Pair>>]| {
            score_sessions(
                Measure::InformationCoefficient,
                model
                    .iter()
                    .enumerate()
                    .map(|(day, instants)| (session(day as i64), &instants[..])),
            )
            .unwrap()
        };
        let (perfect, noise) = (score(&perfect), score(&noise));
        let difference = paired(Treatment(noise.series()), Control(perfect.series())).unwrap();
        assert_eq!(difference.sessions(), 30);
        assert!(difference.t().unwrap() < -10.0, "{difference:?}");
    }

    fn cross_section() -> impl Strategy<Value = Vec<(f64, f64)>> {
        prop::collection::vec((-100i32..100, -100i32..100), 20..60).prop_map(|values| {
            values
                .into_iter()
                .map(|(forecast, outcome)| (f64::from(forecast), f64::from(outcome)))
                .collect()
        })
    }

    fn split(values: &[(f64, f64)]) -> (Vec<f64>, Vec<f64>) {
        values.iter().copied().unzip()
    }

    proptest! {
        /// A coefficient reads only order, so a strictly increasing map of the forecast leaves it unchanged.
        #[test]
        fn test_the_coefficient_reads_only_order(values in cross_section()) {
            let (forecast, outcome) = split(&values);
            let stretched: Vec<f64> = forecast.iter().map(|value| (value / 10.0).exp() - 3.0).collect();
            let coefficient = Measure::InformationCoefficient;
            let (original, mapped) = (
                coefficient.score(&pairs(&forecast, &outcome)),
                coefficient.score(&pairs(&stretched, &outcome)),
            );
            match (original, mapped) {
                (Ok(original), Ok(mapped)) => prop_assert!((original - mapped).abs() < 1e-12),
                (original, mapped) => prop_assert_eq!(original, mapped),
            }
        }

        /// Negating the forecast negates the coefficient and, with every name directional, complements the hit rate.
        #[test]
        fn test_negating_the_forecast_reverses_the_score(values in cross_section()) {
            let (forecast, outcome) = split(&values);
            let negated: Vec<f64> = forecast.iter().map(|value| -value).collect();
            let coefficient = Measure::InformationCoefficient;
            if let (Ok(original), Ok(reversed)) = (
                coefficient.score(&pairs(&forecast, &outcome)),
                coefficient.score(&pairs(&negated, &outcome)),
            ) {
                prop_assert!((original + reversed).abs() < 1e-12);
                prop_assert!((-1.0..=1.0).contains(&original));
            }
            let directional: Vec<(f64, f64)> = values
                .iter()
                .map(|(forecast, outcome)| (forecast + 0.5, outcome + 0.5))
                .collect();
            let (forecast, outcome) = split(&directional);
            let negated: Vec<f64> = forecast.iter().map(|value| -value).collect();
            let original = Measure::HitRate.score(&pairs(&forecast, &outcome)).unwrap();
            let reversed = Measure::HitRate.score(&pairs(&negated, &outcome)).unwrap();
            prop_assert!((original + reversed - 1.0).abs() < 1e-12);
        }
    }

    /// A forecast unrelated to the outcome scores near zero: shuffled forecasts over 400 instants of 50 names average
    /// within four standard errors of zero, while the same machinery reads the planted order as one.
    #[test]
    fn test_a_shuffled_forecast_scores_nothing() {
        let mut generator = Generator::new(17);
        let outcome = line(50);
        let instants: Vec<Vec<Pair>> = (0..400)
            .map(|_| {
                let mut forecast = outcome.clone();
                generator.shuffle(&mut forecast);
                pairs(&forecast, &outcome)
            })
            .collect();
        let scores: Vec<f64> = instants
            .iter()
            .map(|pairs| Measure::InformationCoefficient.score(pairs).unwrap())
            .collect();
        let mean = scores.iter().sum::<f64>() / 400.0;
        let standard_error = (1.0 / 49.0f64).sqrt() / 20.0;
        assert!(mean.abs() < 4.0 * standard_error, "{mean}");
        assert_eq!(
            Measure::InformationCoefficient.score(&pairs(&outcome, &outcome)),
            Ok(1.0)
        );
    }
}
