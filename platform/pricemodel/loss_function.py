from tinygrad import Tensor
from tinygrad.dtype import dtypes  # REMOVE
# import numpy as np


# NOTE: rename all variables here
def quantile_loss(
    y_pred: Tensor,
    y_true: Tensor,
    tickers: Tensor,  # NOTE: remove (?)
    quantiles=[0.25, 0.5, 0.75],
) -> Tensor:
    loss = Tensor(0.0, requires_grad=True)  # .detach()  # TEMP
    for q_idx, q in enumerate(quantiles):
        error = y_true - y_pred[:, :, q_idx]  # (30, 5) vs (30, 5)
        loss_term = (q * error + (q - 1) * error + (error * (2 * q - 1)).abs()) / 2
        loss = loss + loss_term.mean()

    loss = loss.realize()  # keep realize

    return loss


# ------------------------------------------------------------------------------

# def quantile_loss(
#     y_pred: Tensor,
#     y_true: Tensor,
#     tickers: Tensor,
#     quantiles=[0.25, 0.5, 0.75],
# ) -> Tensor:
#     y_true = y_true.cast(dtypes.float32)
#     y_true_mean = y_true.mean(axis=1)  # (30,)

#     # Initialize as list to accumulate, then sum
#     # loss_terms = []
#     loss = Tensor(0.0, requires_grad=True)
#     for q_idx, q in enumerate(quantiles):
#         error = y_true_mean - y_pred[:, q_idx]  # (30,)
#         a = q * error
#         b = (q - 1) * error
#         loss_term = (a + b + (a - b).abs()) / 2  # (30,)
#         # loss_terms.append(loss_term.mean())  # Scalar per quantile
#         loss = loss + loss_term.mean()  # Scalar addition

#     # Sum all terms at once
#     # loss = Tensor.stack(*loss_terms).sum()  # Scalar, requires_grad=True
#     print("loss - final loss.shape:", loss.shape, "requires_grad:", loss.requires_grad)  # TEMP

#     return loss.reshape(())
