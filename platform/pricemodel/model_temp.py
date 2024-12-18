import polars as pl
import json
from tinygrad import Tensor
from tinygrad.nn import Linear


# NOTE (TEMP):
# using this as a blueprint
# https://nixtlaverse.nixtla.io/neuralforecast/models.tft.html


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

    def __iter__(self):
        for i in range(0, self.n_samples, self.batch_size):
            batch = self.data.slice(i, i + self.batch_size)
            yield batch


class GatedResidualNetwork:
    def __init__(
        self,
        input_size: int,
        hidden_size: int,
        output_size: int,
        dropout_rate: float,  # (?)
    ) -> None:
        self.fc1 = Linear(input_size, hidden_size)
        self.fc2 = Linear(hidden_size, hidden_size)
        self.gate = Tensor.uniform(hidden_size, output_size)  # (?)
        self.dropout_rate = dropout_rate

    def forward(
        self,
        x: Tensor,
        residual: Tensor = None,
    ) -> Tensor:
        x = self.fc1(x)
        x = x.elu()
        x = x.dropout(self.dropout_rate)
        x = self.fc2(x)
        x = self.gate(x, residual)

        return x


# NOTE: incomplete
class VariableSelectionNetwork:
    def __init__(
        self,
        input_size: int,
        hidden_size: int,
        output_size: int,
        dropout_rate: float,
    ) -> None:
        self.fc1 = Linear(input_size, hidden_size)
        self.fc2 = Linear(hidden_size, hidden_size)
        self.fc3 = Linear(hidden_size, output_size)
        self.dropout_rate = dropout_rate

        self.grns = [
            GatedResidualNetwork(hidden_size, hidden_size, output_size, dropout_rate)
            for _ in range(output_size)
        ]

    def forward(
        self,
        x: Tensor,
    ) -> Tensor:
        x = self.fc1(x)
        x = x.elu()
        x = x.dropout(self.dropout_rate)
        x = self.fc2(x)
        x = x.elu()
        x = x.dropout(self.dropout_rate)
        x = self.fc3(x)

        return x


# class StaticCovariateEncoder:
#     def __init__(
#         self,
#     ) -> None:
#         pass  # TEMP

#     def forward(
#         self,
#         x: Tensor,
#     ) -> Tensor:
#         pass  # TEMP


# TEMP --------------------------------------------

df = pl.read_csv("consolidated_data.csv")

preprocessor = Preprocessor(df)

preprocessed_data = preprocessor.normalize(df)

data_loader = DataLoader(preprocessed_data)

for batch in data_loader:
    print(batch)
    break


# TEMP --------------------------------------------

# outline:
# [ ] temporal fusion transformer class
# - [ ] methods
# - - [ ] train model
# - - [ ] validate model
# - - [ ] save model
# - - [ ] load model
# - - [ ] invoke model
# [ ] types
# - [x] preprocessor
# - [x] data loader
# - [ ] static encoder
# - [ ] temporal encoder
# - [ ] gated residual network
# - [ ] multi-head attention
# - [ ] output layer
# [ ] hyperparameters
# - [ ] optimizer
# - [ ] learning rate
# - [ ] batch size
# - [ ] epochs
# - [ ] early stopping
