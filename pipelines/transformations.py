import numpy as np
import polars as pl
from prefect import task

from pipelines.types import ColumnSubset


@task
def drop_nulls(data: pl.DataFrame, subset: ColumnSubset | None = None) -> pl.DataFrame:
    if subset:
        return data.drop_nulls(subset=subset)

    return data.drop_nulls()


@task
def drop_duplicates(
    data: pl.DataFrame, subset: ColumnSubset | None = None
) -> pl.DataFrame:
    if subset:
        return data.unique(subset=subset)
    return data.unique()


@task
def select_columns(
    data: pl.DataFrame, subset: ColumnSubset | None = None
) -> pl.DataFrame:
    if len(subset) == 1:
        return data.select(subset).to_numpy().reshape(-1).tolist()
    return data.select(subset)


@task
def group_data(data: pl.DataFrame, subset: ColumnSubset):
    return data.groupby(subset)


@task
def filter_data_by_column_value(
    data: pl.DataFrame, column: str, value: str
) -> pl.DataFrame:
    return data.filter(pl.col(column) == value)


@task
def sort_by_columns(
    data: pl.DataFrame, subset: ColumnSubset, descending: bool | list[bool] = False
) -> pl.DataFrame:
    return data.sort(subset, descending=descending)


@task
def to_ndarray(data: pl.DataFrame, subset: ColumnSubset) -> np.ndarray:
    return data.select(subset).to_numpy()


@task
def min_max_scaler(
    data: pl.DataFrame, feature_range: tuple[float, float] = (0, 1)
) -> pl.DataFrame:
    def _scaler(column, feature_range: tuple[float, float] = (0, 1)):
        min_ = column.min()
        max_ = column.max()
        scaled_column = (column - min_) / (max_ - min_)
        return scaled_column * (feature_range[1] - feature_range[0]) + feature_range[0]

    return data.select(
        [
            _scaler(col, feature_range) if col.dtype in [pl.Float64, pl.Int64] else col
            for col in data
        ]
    )
