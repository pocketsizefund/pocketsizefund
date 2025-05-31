from tinygrad.tensor import Tensor


def quantile_loss(y_pred: Tensor, y_true: Tensor, quantiles=None):
    if quantiles is None:
        quantiles = [0.25, 0.5, 0.75]
    # …rest of implementation…
    loss = Tensor.zeros(1)
    for q in quantiles:
        error = y_true - y_pred
        loss += Tensor.maximum(q * error, (q - 1) * error).mean()

    return loss
