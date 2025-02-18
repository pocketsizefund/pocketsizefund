from pricemodel.loss_function import quantile_loss
from tinygrad import Tensor


def test_loss_function():
    y_pred = Tensor([1, 2, 3, 4])
    y_true = Tensor([0, 0, 0, 0])
    loss = quantile_loss(y_pred, y_true)

    assert loss.numpy() == 3.75
