"""Data Transformation Tools.

This module provides a set of functions designed to perform common data manipulation
tasks using Polars DataFrames. It includes tools for scaling data, dropping nulls,
removing duplicates, selecting specific columns, filtering data by column values, and
sorting data based on one or multiple columns.

Each function is tailored to work efficiently with Polars DataFrame objects and is
designed to handle both simple and complex data manipulation scenarios. These functions
are useful for data preprocessing steps in data analysis and machine learning workflows.
"""

import polars as pl
from prefect import task

from pipelines.types import ColumnSubset


@task
def drop_nulls(data: pl.DataFrame, subset: ColumnSubset | None = None) -> pl.DataFrame:
    """Remove rows from the DataFrame that contain null values.

    If a subset of columns is specified, only rows with nulls in these columns are dropped.
    If no subset is provided, any row containing at least one null value is removed.

    Args:
    ----
        data(pl.DataFrame): The Polars DataFrame from which to remove null rows.
        subset(ColumnSubset | None, optional): Specific columns to consider for null checks.
            Defaults to None, which checks all columns.

    Returns:
    -------
        pl.DataFrame: A DataFrame with null rows dropped based on the specified conditions.

    Examples:
    --------
        >> > import polars as pl
        >> > df = pl.DataFrame({
        >> >     "A": [1, None, 3],
        >> >     "B": [4, 5, None]
        >> >})
        >> > print(drop_nulls(df))

    """
    if subset:
        return data.drop_nulls(subset=subset)

    return data.drop_nulls()


@task
def drop_duplicates(
    data: pl.DataFrame,
    subset: ColumnSubset | None = None,
) -> pl.DataFrame:
    """Remove duplicate rows from the DataFrame, optionally considering only a subset of columns.

    If a subset of columns is specified, duplicates are identified based on these columns only.
    If no subset is provided, duplicates are identified based on all columns.

    Args:
    ----
        data(pl.DataFrame): The DataFrame from which to remove duplicates.
        subset(ColumnSubset | None, optional): Columns to consider when identifying duplicates.
            Defaults to None, considering all columns.

    Returns:
    -------
        pl.DataFrame: A DataFrame with duplicates removed based on the specified conditions.

    Examples:
    --------
        >> > import polars as pl
        >> > df = pl.DataFrame({
        >> >     "A": [1, 1, 2],
        >> >     "B": [3, 3, 4]
        >> >})
        >> > print(drop_duplicates(df))

    """
    if subset:
        return data.unique(subset=subset)
    return data.unique()


@task
def select_columns(
    data: pl.DataFrame,
    subset: ColumnSubset | None = None,
) -> pl.DataFrame:
    """Select specific columns from the DataFrame.

    If a single column is selected, this function returns the data as a flat list.
    For multiple columns, it returns a DataFrame consisting only of the specified columns.

    Args:
        data(pl.DataFrame): The DataFrame from which to select columns.
        subset(ColumnSubset | None, optional): The columns to select from the DataFrame.

    Returns:
        pl.DataFrame | list: A DataFrame or list containing only the selected columns.

    Examples:
        >> > import polars as pl
        >> > df = pl.DataFrame({
        >> >     "A": [1, 2, 3],
        >> >     "B": [4, 5, 6]
        >> >})
        >> > print(select_columns(df, ["A"]))

    """
    if not subset:
        return data
    return data.select(subset)


@task
def filter_data_by_column_value(
    data: pl.DataFrame,
    column: str,
    value: str,
) -> pl.DataFrame:
    """Filter the DataFrame to include only rows
    where the specified column matches the given value.

    Args:
        data(pl.DataFrame): The DataFrame to filter.
        column(str): The column to filter by.
        value(str): The value to match in the specified column.

    Returns:
        pl.DataFrame: A DataFrame containing only the rows
                      that match the specified value
                      in the given column.

    Examples:
        >> > import polars as pl
        >> > df = pl.DataFrame({
        >> >     "A": ["foo", "bar", "baz"],
        >> >     "B": [1, 2, 3]
        >> >})
        >> > print(filter_data_by_column_value(df, "A", "bar"))

    """
    return data.filter(pl.col(column) == value)


@task
def sort_by_columns(
    data: pl.DataFrame,
    subset: ColumnSubset,
    descending: bool | list[bool] = False,
) -> pl.DataFrame:
    """Filter the DataFrame to include only rows where the specified column matches the given value.

    Args:
    ----
        data(pl.DataFrame): The DataFrame to filter.
        column(str): The column to filter by.
        value(str): The value to match in the specified column.

    Returns:
    -------
        pl.DataFrame: A DataFrame containing only the rows that match the specified value in the given column.

    Examples:
    --------
        >> > import polars as pl
        >> > df = pl.DataFrame({
        >> >     "A": ["foo", "bar", "baz"],
        >> >     "B": [1, 2, 3]
        >> >})
        >> > print(filter_data_by_column_value(df, "A", "bar"))

    """
    return data.sort(subset, descending=descending)


@task
def min_max_scaler(
    data: pl.DataFrame,
    feature_range: tuple[float, float] = (0, 1),
) -> pl.DataFrame:
    """Scale numerical columns of a Polars DataFrame to a specified range.

    This function scales each numerical column in the input DataFrame to a range
    defined by `feature_range`. It applies the Min-Max scaling technique which
    transforms data to a scale from the minimum value to the maximum value of
    the feature range.

    Args:
    ----
        data(pl.DataFrame): The Polars DataFrame containing the data to scale.
        feature_range(tuple[float, float], optional): The target range that the
            data will be scaled to. Defaults to(0, 1), which scales data to the
            range 0 to 1.

    Returns:
    -------
        pl.DataFrame: A new DataFrame with scaled numerical columns. Non-numerical
        columns are not modified and are included in the returned DataFrame.

    Examples:
    --------
        >> > import polars as pl
        >> > df = pl.DataFrame({
        >> >     "A": [1, 2, 3],
        >> >     "B": [4, 5, 6]
        >> >})
        >> > print(min_max_scaler(df, (0, 1)))

    """

    def _scaler(column: pl.Series, feature_range: tuple[float, float] = (0, 1)) -> pl.Series:
        min_ = column.min()
        max_ = column.max()
        # Check if all values are the same
        if max_ == min_:
            # All values become the lower bound of the feature range
            return pl.Series(column.name, [feature_range[0]] * len(column))
        scaled_column = (column - min_) / (max_ - min_)
        return scaled_column * (feature_range[1] - feature_range[0]) + feature_range[0]

    return data.select(
        [
            _scaler(col, feature_range) if col.dtype in [pl.Float64, pl.Int64] else col
            for col in data
        ],
    )
