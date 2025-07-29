from tinygrad.tensor import Tensor


def quantile_loss(
    predictions: Tensor,  # shape: (batch_size, output_size, len(quantiles))
    targets: Tensor,  # shape: (batch_size, output_size)
    quantiles: list[float] | None = None,
) -> Tensor:
    if quantiles is None:
        quantiles = [0.1, 0.5, 0.9]

    if not all(0 <= q <= 1 for q in quantiles):
        message = "All quantiles must be between 0 and 1"
        raise ValueError(message)

    errors_total = Tensor.zeros((1,))
    for index, quantile in enumerate(quantiles):
        error = targets.sub(predictions[:, :, index])
        quantile_tensor = Tensor([quantile])
        errors_total = errors_total.add(
            Tensor.where(
                error > 0,
                quantile_tensor.mul(error),
                (quantile_tensor.sub(1)).mul(error),
            ).mean()
        )

    return errors_total.div(len(quantiles))  # shape: (1,)
