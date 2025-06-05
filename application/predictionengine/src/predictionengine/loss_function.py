from typing import cast

from tinygrad.tensor import Tensor

Quantiles = tuple[float, float, float] | tuple[float, float, float, float, float]


def quantile_loss(
    y_pred: Tensor, y_true: Tensor, quantiles: Quantiles | None = None
) -> Tensor:
    if quantiles is None:
        quantiles = (0.25, 0.5, 0.75)

    if y_pred.shape != y_true.shape:
        message = f"Shape mismatch: y_pred {y_pred.shape} vs y_true {y_true.shape}"
        raise ValueError(message)

    if not all(0 <= q <= 1 for q in quantiles):
        message = "All quantiles must be between 0 and 1"
        raise ValueError(message)

    loss: Tensor = Tensor.zeros(1)
    error = cast("Tensor", y_true - y_pred)
    for quantile in quantiles:
        quantile_error = cast("Tensor", quantile * error)
        quantile_minus_one_error = cast("Tensor", (quantile - 1) * error)
        loss += Tensor.maximum(quantile_error, quantile_minus_one_error).mean()

    return loss
