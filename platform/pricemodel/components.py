from tinygrad import Tensor
import numpy as np
import polars as pl
# from typing import Dict, Tuple


class Normalizer:
    def __init__(
        self,
        data: pl.DataFrame,
    ) -> None:
        self.mean = data.mean(axis=0)
        self.standard_deviation = data.std(axis=0)

    def normalize(
        self,
        data: pl.DataFrame,
    ) -> pl.DataFrame:
        return (data - self.mean) / self.standard_deviation

    def save_parameters(
        self,
        path: str,
    ) -> None:
        np.savez(path, mean=self.mean.numpy(), std=self.standard_deviation.numpy())

    def load_parameters(
        self,
        path: str,
    ) -> any:  # TEMP (NEEDS TYPE)
        parameters = np.load(path)
        normalizer = Normalizer(None)
        normalizer.mean = Tensor(parameters["mean"])
        normalizer.standard_deviation = Tensor(parameters["standard_deviation"])
        return normalizer


class DataLoader:
    def __init__(
        self,
        dataframe: pl.DataFrame,
        batch_size: int = 64,
    ) -> None:
        self.dataframe = dataframe
        self.batch_size = batch_size
        self.n_samples = len(dataframe)

    def __iter__(self):
        for i in range(0, self.n_samples, self.batch_size):
            batch = self.dataframe.iloc[i : i + self.batch_size]
            yield Tensor(batch)


# class VariableSelectionNetwork:
#     def __init__(
#         self,
#         # input_sizes: Dict[str, int],
#         # hidden_size: int,
#         # input_embedding_flags: Dict[str, bool] = None,
#         # dropout: float = 0.1,
#         # context_size: int = None,
#         # single_variable_grns: Dict[str, GatedResidualNetwork] = None,
#         # prescalers: Dict[str, nn.Linear] = None,
#     ) -> None:
#         pass  # TEMP

#     def __call__(
#         self,
#         x: Dict[str, Tensor],
#         context: Tensor = None,
#     ) -> any:  # TEMP (NEEDS TYPE)
#         pass  # TEMP


# class GatedResidualNetwork:
#     def __init__(
#         self,
#         input_size: int,
#         hidden_size: int,
#         output_size: int,
#         dropout: float = 0.1,
#         context_size: int = None,
#         residual: bool = False,
#     ) -> None:
#         pass  # TEMP


# class ResampleNorm:
#     def __init__() -> None:
#         pass
