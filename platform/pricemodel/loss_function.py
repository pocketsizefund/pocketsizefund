from tinygrad import Tensor


def loss_function(predictions: Tensor, targets: Tensor) -> Tensor:
    return ((predictions - targets) ** 2).mean()  # MSE loss
