from tinygrad.tensor import Tensor

Quantiles = tuple[float, float, float] | tuple[float, float, float, float, float]


def quantile_loss(
    y_pred: Tensor, y_true: Tensor, quantiles: Quantiles | None = None
) -> Tensor:
    if quantiles is None:
        quantiles = (0.25, 0.5, 0.75)
    loss: Tensor = Tensor.zeros(1)
    for q in quantiles:
        error: Tensor = y_true - y_pred
        loss += Tensor.maximum(q * error, (q - 1) * error).mean()

    return loss
