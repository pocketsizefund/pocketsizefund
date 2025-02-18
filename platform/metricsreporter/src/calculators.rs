use chrono::{DateTime, Duration, Utc};
use serde::{Deserialize, Serialize};
use statrs::statistics::{Data, Statistics};

const WEEKLY_DURATION: i64 = 5;
const MONTHLY_DURATION: i64 = 20;
const YEARLY_DURATION: i64 = 252;

#[derive(Debug, Deserialize, Serialize)]
pub struct Metrics {
    one_week_returns: f64,
    one_month_returns: f64,
    one_year_returns: f64,
    annualized_returns: f64,
    maximum_drawdown: f64,
    daily_sharpe_ratio: f64,
    daily_sortino_ratio: f64,
    daily_mean_annualized: f64,
    daily_volatility_annualized: f64,
    daily_skewness: f64,
    daily_kurtosis: f64,
}

pub fn calculate_metrics(
    timestamps: Vec<DateTime<Utc>>,
    daily_closing_prices: Vec<f64>,
    current_time: DateTime<Utc>,
) -> Option<Metrics> {
    let sorted_daily_closing_prices_by_timestamp =
        sort_daily_closing_prices_by_timestamp(timestamps, daily_closing_prices);

    let sorted_daily_closing_prices_by_timestamp: Vec<(DateTime<Utc>, f64)> =
        sorted_daily_closing_prices_by_timestamp
            .into_iter()
            .filter(|(timestamp, _)| *timestamp <= current_time)
            .collect();

    let one_week_returns = calculate_returns(
        sorted_daily_closing_prices_by_timestamp.clone(),
        current_time,
        WEEKLY_DURATION,
    );

    let one_month_returns = calculate_returns(
        sorted_daily_closing_prices_by_timestamp.clone(),
        current_time,
        MONTHLY_DURATION,
    );

    let one_year_returns = calculate_returns(
        sorted_daily_closing_prices_by_timestamp.clone(),
        current_time,
        YEARLY_DURATION,
    );

    let annualized_returns = calculate_annualized_returns(one_week_returns?);

    let daily_returns: Vec<f64> = sorted_daily_closing_prices_by_timestamp
        .iter()
        .map(|&(_, value)| value)
        .collect();

    let maximum_drawdown = calculate_maximum_drawdown(daily_returns.clone())?;

    let daily_sharpe_ratio = daily_sharpe_ratio(daily_returns.clone())?;

    let daily_sortino_ratio = daily_sortino_ratio(daily_returns.clone())?;

    let daily_mean_annualized = daily_mean_annualized(daily_returns.clone())?;

    let daily_volatility_annualized = daily_volatility_annualized(daily_returns.clone())?;

    let daily_skewness = daily_skew(daily_returns.clone())?;

    let daily_kurtosis = daily_kurtosis(daily_returns.clone())?;

    Some(Metrics {
        one_week_returns: one_week_returns?,
        one_month_returns: one_month_returns?,
        one_year_returns: one_year_returns?,
        annualized_returns,
        maximum_drawdown,
        daily_sharpe_ratio,
        daily_sortino_ratio,
        daily_mean_annualized,
        daily_volatility_annualized,
        daily_skewness,
        daily_kurtosis,
    })
}

fn sort_daily_closing_prices_by_timestamp(
    timestamps: Vec<DateTime<Utc>>,
    daily_closing_prices: Vec<f64>,
) -> Vec<(DateTime<Utc>, f64)> {
    let mut combined: Vec<(DateTime<Utc>, f64)> =
        timestamps.into_iter().zip(daily_closing_prices).collect();

    combined.sort_by_key(|&(timestamp, _)| timestamp);

    combined
}

fn calculate_returns(
    daily_closing_prices_by_timestamp: Vec<(DateTime<Utc>, f64)>,
    current_time: DateTime<Utc>,
    duration_days: i64,
) -> Option<f64> {
    let target_timestamp = current_time - Duration::days(duration_days);

    let (start, end) = match (
        daily_closing_prices_by_timestamp
            .iter()
            .find(|&&(ts, _)| ts >= target_timestamp),
        daily_closing_prices_by_timestamp.iter().last(),
    ) {
        (Some(&(_, start_eq)), Some(&(end_ts, end_eq))) if end_ts <= current_time => {
            (start_eq, end_eq)
        }
        _ => return None, // Not enough data
    };

    Some((end - start) / start)
}

fn calculate_annualized_returns(one_week_returns: f64) -> f64 {
    (1.0 + one_week_returns).powi(52_i32) - 1.0
}

fn calculate_maximum_drawdown(daily_returns: Vec<f64>) -> Option<f64> {
    let mut max_drawdown = 0.0;
    let mut peak = daily_returns[0];
    for daily_return in daily_returns {
        if daily_return > peak {
            peak = daily_return;
        }

        let drawdown = (peak - daily_return) / peak;
        if drawdown > max_drawdown {
            max_drawdown = drawdown;
        }
    }

    Some(max_drawdown)
}

fn daily_sharpe_ratio(daily_returns: Vec<f64>) -> Option<f64> {
    let mean = daily_returns.clone().mean();
    let standard_deviation = daily_returns.clone().std_dev();
    Some((mean / standard_deviation) * (YEARLY_DURATION as f64).sqrt())
}

fn daily_sortino_ratio(daily_returns: Vec<f64>) -> Option<f64> {
    let mean = daily_returns.clone().mean();
    let downside_returns: Vec<f64> = daily_returns.iter().cloned().filter(|&r| r < 0.0).collect();
    let downside_deviation = downside_returns.std_dev();
    Some((mean / downside_deviation) * (YEARLY_DURATION as f64).sqrt())
}

fn daily_mean_annualized(daily_returns: Vec<f64>) -> Option<f64> {
    let daily_mean = daily_returns.mean();
    Some(daily_mean * YEARLY_DURATION as f64)
}

fn daily_volatility_annualized(daily_returns: Vec<f64>) -> Option<f64> {
    let daily_vol = daily_returns.std_dev();
    Some(daily_vol * (YEARLY_DURATION as f64).sqrt())
}

fn daily_skew(daily_returns: Vec<f64>) -> Option<f64> {
    let mean: f64 = daily_returns.iter().sum::<f64>() / daily_returns.len() as f64;

    let variance = daily_returns
        .iter()
        .map(|value| (value - mean).powi(2))
        .sum::<f64>()
        / daily_returns.len() as f64;

    let standard_deviation = variance.sqrt();

    let number = daily_returns.len() as f64;

    let skewness_sum: f64 = daily_returns
        .iter()
        .map(|&value| ((value - mean) / standard_deviation).powi(3))
        .sum();

    Some((number / ((number - 1.0) * (number - 2.0))) * skewness_sum)
}

fn daily_kurtosis(daily_returns: Vec<f64>) -> Option<f64> {
    let daily_returns_data = Data::new(daily_returns.clone());
    let number = daily_returns_data.len() as f64;
    let mean = daily_returns_data.iter().sum::<f64>() / number;
    let variance = daily_returns_data
        .iter()
        .map(|x| (x - mean).powi(2))
        .sum::<f64>()
        / number;
    let fourth_moment = daily_returns_data
        .iter()
        .map(|x| (x - mean).powi(4))
        .sum::<f64>()
        / number;
    Some(fourth_moment / variance.powi(2) - 3.0)
}

#[cfg(test)]
mod tests {
    use super::*;
    use chrono::{DateTime, TimeZone};

    #[test]
    fn test_sort_daily_closing_prices_by_timestamp() {
        let timestamps: Vec<DateTime<Utc>> = vec![
            Utc.with_ymd_and_hms(2021, 1, 3, 0, 0, 0).unwrap(),
            Utc.with_ymd_and_hms(2021, 1, 2, 0, 0, 0).unwrap(),
            Utc.with_ymd_and_hms(2021, 1, 4, 0, 0, 0).unwrap(),
            Utc.with_ymd_and_hms(2021, 1, 5, 0, 0, 0).unwrap(),
            Utc.with_ymd_and_hms(2021, 1, 6, 0, 0, 0).unwrap(),
            Utc.with_ymd_and_hms(2021, 1, 7, 0, 0, 0).unwrap(),
            Utc.with_ymd_and_hms(2021, 1, 8, 0, 0, 0).unwrap(),
            Utc.with_ymd_and_hms(2021, 1, 9, 0, 0, 0).unwrap(),
            Utc.with_ymd_and_hms(2021, 1, 1, 0, 0, 0).unwrap(),
            Utc.with_ymd_and_hms(2021, 1, 10, 0, 0, 0).unwrap(),
        ];

        let daily_closing_prices = vec![3.0, 2.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0, 1.0, 10.0];

        let sorted_daily_closing_prices_by_timestamp =
            sort_daily_closing_prices_by_timestamp(timestamps, daily_closing_prices);

        assert_eq!(
            sorted_daily_closing_prices_by_timestamp[0],
            (Utc.with_ymd_and_hms(2021, 1, 1, 0, 0, 0).unwrap(), 1.0)
        );
        assert_eq!(
            sorted_daily_closing_prices_by_timestamp[1],
            (Utc.with_ymd_and_hms(2021, 1, 2, 0, 0, 0).unwrap(), 2.0)
        );
    }

    #[test]
    fn test_calculate_returns() {
        let daily_closing_prices_by_timestamp: Vec<(DateTime<Utc>, f64)> = vec![
            (Utc.with_ymd_and_hms(2021, 1, 1, 0, 0, 0).unwrap(), 1.0),
            (Utc.with_ymd_and_hms(2021, 1, 2, 0, 0, 0).unwrap(), 2.0),
            (Utc.with_ymd_and_hms(2021, 1, 3, 0, 0, 0).unwrap(), 3.0),
            (Utc.with_ymd_and_hms(2021, 1, 4, 0, 0, 0).unwrap(), 4.0),
            (Utc.with_ymd_and_hms(2021, 1, 5, 0, 0, 0).unwrap(), 5.0),
            (Utc.with_ymd_and_hms(2021, 1, 6, 0, 0, 0).unwrap(), 6.0),
            (Utc.with_ymd_and_hms(2021, 1, 7, 0, 0, 0).unwrap(), 7.0),
            (Utc.with_ymd_and_hms(2021, 1, 8, 0, 0, 0).unwrap(), 8.0),
            (Utc.with_ymd_and_hms(2021, 1, 9, 0, 0, 0).unwrap(), 9.0),
            (Utc.with_ymd_and_hms(2021, 1, 10, 0, 0, 0).unwrap(), 10.0),
        ];

        let current_time = Utc.with_ymd_and_hms(2021, 1, 10, 0, 0, 0).unwrap();

        let returns = calculate_returns(
            daily_closing_prices_by_timestamp,
            current_time,
            WEEKLY_DURATION,
        );

        match returns {
            None => panic!("Returns calculation failed"),
            Some(returns) => {
                assert_eq!(returns, 1.0);
            }
        }
    }

    #[test]
    fn test_calculate_annualized_returns() {
        let one_week_returns = 0.01;

        let annualized_returns = calculate_annualized_returns(one_week_returns);

        assert_eq!(annualized_returns, 0.6776889214629449);
    }

    #[test]
    fn test_calculate_maximum_drawdown() {
        let daily_returns = vec![0.01, 0.02, 0.03, 0.04, 0.05, 0.06, 0.07, 0.02, 0.09, 0.1];

        let maximum_drawdown = calculate_maximum_drawdown(daily_returns);

        match maximum_drawdown {
            None => panic!("Maximum drawdown calculation failed"),
            Some(maximum_drawdown) => {
                assert_eq!(maximum_drawdown, 0.7142857142857143);
            }
        }
    }

    #[test]
    fn test_daily_sharpe_ratio() {
        let daily_returns = vec![0.01, 0.02, 0.03, 0.04, 0.05, 0.06, 0.07, 0.02, 0.09, 0.1];

        let sharpe_ratio = daily_sharpe_ratio(daily_returns);

        match sharpe_ratio {
            None => panic!("Sharpe ratio calculation failed"),
            Some(sharpe_ratio) => {
                assert_eq!(sharpe_ratio, 25.325834238158027);
            }
        }
    }

    #[test]
    fn test_daily_sortino_ratio() {
        let daily_returns = vec![0.01, 0.02, 0.03, -0.04, 0.05, 0.06, -0.07, 0.02, 0.09, 0.1];

        let sortino_ratio = daily_sortino_ratio(daily_returns);

        match sortino_ratio {
            None => panic!("Sortino ratio calculation failed"),
            Some(sortino_ratio) => {
                assert_eq!(sortino_ratio, 20.204949888579286);
            }
        }
    }

    #[test]
    fn test_daily_mean_annualized() {
        let daily_returns = vec![0.01, 0.02, 0.03, 0.04, 0.05, 0.06, 0.07, 0.02, 0.09, 0.1];

        let mean_annualized = daily_mean_annualized(daily_returns);

        match mean_annualized {
            None => panic!("Mean annualized calculation failed"),
            Some(mean_annualized) => {
                assert_eq!(mean_annualized, 12.348);
            }
        }
    }

    #[test]
    fn test_daily_volatility_annualized() {
        let daily_returns = vec![0.01, 0.02, 0.03, 0.04, 0.05, 0.06, 0.07, 0.02, 0.09, 0.1];

        let volatility_annualized = daily_volatility_annualized(daily_returns);

        match volatility_annualized {
            None => panic!("Volatility annualized calculation failed"),
            Some(volatility_annualized) => {
                assert_eq!(volatility_annualized, 0.4875653802312055);
            }
        }
    }

    #[test]
    fn test_daily_skew() {
        let daily_returns = vec![0.01, 0.02, 0.03, 0.04, 0.05, 0.06, 0.07, 0.02, 0.09, 0.1];

        let skew = daily_skew(daily_returns);

        match skew {
            None => panic!("Skew calculation failed"),
            Some(skew) => {
                assert_eq!(skew, 0.5416805764977253);
            }
        }
    }

    #[test]
    fn test_daily_kurtosis() {
        let daily_returns = vec![0.01, 0.02, 0.03, 0.04, 0.05, 0.06, 0.07, 0.02, 0.09, 0.1];

        let kurtosis = daily_kurtosis(daily_returns);

        match kurtosis {
            None => panic!("Kurtosis calculation failed"),
            Some(kurtosis) => {
                assert_eq!(kurtosis, -1.1041965813032997);
            }
        }
    }
}
