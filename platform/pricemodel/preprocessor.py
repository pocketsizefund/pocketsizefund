import polars as pl
import json
from tinygrad import Tensor
from typing import Iterable


class Preprocessor:
    def __init__(self, data: pl.DataFrame) -> None:
        self.skipped_columns = ["ticker", "timestamp"]

        filtered_data = data.drop(self.skipped_columns)

        self.means = filtered_data.select(
            [pl.col(c).mean().alias(c) for c in filtered_data.columns]
        )

        self.standard_deviations = filtered_data.select(
            [pl.col(c).std().alias(c) for c in filtered_data.columns]
        )

    def save_parameters(self) -> None:
        output = {
            "means": self.means.to_dict(),
            "standard_deviations": self.standard_deviations.to_dict(),
        }

        with open("normalization_parameters.json", "w") as json_file:
            json.dump(output, json_file, indent=4)

    def load_parameters(self) -> None:
        with open("normalization_parameters.json", "r") as json_file:
            input = json.load(json_file)

            self.means = pl.DataFrame(input["means"])
            self.standard_deviations = pl.DataFrame(input["standard_deviations"])

    def normalize(self, data: pl.DataFrame) -> pl.DataFrame:
        if self.means is None or self.standard_deviations is None:
            raise ValueError("Normalization parameters have not been set")

        filtered_data = data.drop(self.skipped_columns)

        textual_data = data.select(self.skipped_columns)

        temporal_data = filtered_data.select(
            [
                (pl.col(c) - self.means[c]) / self.standard_deviations[c]
                for c in filtered_data.columns
            ]
        )

        normalized_data = pl.concat([textual_data, temporal_data], how="horizontal")

        return normalized_data


class DataLoader:
    def __init__(self, data: pl.DataFrame, batch_size: int = 64) -> None:
        self.data = data
        self.batch_size = batch_size
        self.n_samples = len(data)

    def __iter__(self) -> Iterable[Tensor]:
        for i in range(0, self.n_samples, self.batch_size):
            batch = self.data.slice(i, i + self.batch_size)
            yield Tensor(batch.to_numpy())
