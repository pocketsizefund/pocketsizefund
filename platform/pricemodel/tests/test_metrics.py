from pricemodel.metrics import smape, mape, mae, rmse, quantile_loss
import numpy as np


def test_smape():
    actual_values = [100, 200, 300, 400, 500]
    predicted_values = [110, 190, 290, 410, 490]

    output = smape(np.array(actual_values), np.array(predicted_values))

    assert output == np.float64(4.506236596632076)


def test_mape():
    actual_values = [100, 200, 300, 400, 500]
    predicted_values = [110, 190, 290, 410, 490]

    output = mape(np.array(actual_values), np.array(predicted_values))

    assert output == np.float64(4.566666666666666)


def test_mae():
    actual_values = [100, 200, 300, 400, 500]
    predicted_values = [110, 190, 290, 410, 490]

    output = mae(np.array(actual_values), np.array(predicted_values))

    assert output == np.float64(10.0)


def test_rmse():
    actual_values = [100, 200, 300, 400, 500]
    predicted_values = [110, 190, 290, 410, 490]

    output = rmse(np.array(actual_values), np.array(predicted_values))

    assert output == np.float64(10.0)


def test_quantile_loss():
    actual_values = [100, 200, 300, 400, 500]
    predicted_values = [110, 190, 290, 410, 490]
    output_values = [4.5, 5.0, 5.5]

    for i, q in enumerate([0.25, 0.5, 0.75]):
        output = quantile_loss(actual_values, predicted_values, quantile=q)
        assert output == np.float64(output_values[i])
