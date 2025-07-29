import polars as pl
from flytekit import task  # , workflow


@task
def read_local_data(filepath: str) -> pl.DataFrame:
    return pl.read_csv(filepath)


def train_model(
    data: pl.DataFrame,
    # epoch_count: int = 10,  # noqa: ERA001
    # learning_rate: float = 1e-3,  # noqa: ERA001
) -> None:  # TemporalFusionTransformer:
    if data.is_empty():
        message = "No data provided for training"
        raise ValueError(message)


# outline:
# [x] read local data
# [ ] train model
# [ ] save model artifact
