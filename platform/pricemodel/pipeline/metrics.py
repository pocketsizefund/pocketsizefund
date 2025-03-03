import numpy as np


def smape(actual: np.ndarray, predicted: np.ndarray) -> float:
    actual, predicted = np.array(actual), np.array(predicted)
    denominator = np.abs(actual) + np.abs(predicted)
    smape = np.mean(2 * np.abs(actual - predicted) / denominator) * 100
    return smape


def mape(actual: np.ndarray, predicted: np.ndarray) -> float:
    actual, predicted = np.array(actual), np.array(predicted)
    mape = np.mean(np.abs((actual - predicted) / actual)) * 100
    return mape


def mae(actual: np.ndarray, predicted: np.ndarray) -> float:
    actual, predicted = np.array(actual), np.array(predicted)
    mae = np.mean(np.abs(actual - predicted))
    return mae


def rmse(actual: np.ndarray, predicted: np.ndarray) -> float:
    actual, predicted = np.array(actual), np.array(predicted)
    rmse = np.sqrt(np.mean((actual - predicted) ** 2))
    return rmse


def quantile_loss(y_true: np.ndarray, y_pred: np.ndarray, quantile: float) -> float:
    assert 0 < quantile < 1, "Quantile must be between 0 and 1."
    y_true, y_pred = np.array(y_true), np.array(y_pred)
    diff = y_true - y_pred
    loss = np.maximum(quantile * diff, (quantile - 1) * diff)
    return np.mean(loss)
