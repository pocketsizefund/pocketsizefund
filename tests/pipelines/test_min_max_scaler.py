import polars as pl
import pytest

from pipelines.transformations import min_max_scaler

dataframe = pl.DataFrame(
    {"A": [1, 2, 3, 4, 5], "B": [10, 20, 30, 40, 50], "C": ["x", "y", "z", "w", "v"]},
)


@pytest.mark.parametrize(
    "input_data, feature_range, expected_output",
    [
        # scale to [0,1], full dataset, excludes non-numeric columns
        (
            dataframe,
            (0, 1),
            pl.DataFrame(
                {
                    "A": [0, 0.25, 0.5, 0.75, 1],
                    "B": [0, 0.25, 0.5, 0.75, 1],
                    "C": ["x", "y", "z", "w", "v"],
                },
            ),
        ),
        # scale to [-1, 1], full dataset
        (
            dataframe,
            (-1, 1),
            pl.DataFrame(
                {
                    "A": [-1, -0.5, 0, 0.5, 1],
                    "B": [-1, -0.5, 0, 0.5, 1],
                    "C": ["x", "y", "z", "w", "v"],
                },
            ),
        ),
        # scale to [0,1], subset of columns
        (
            dataframe.select(["A", "B"]),
            (0, 1),
            pl.DataFrame({"A": [0, 0.25, 0.5, 0.75, 1], "B": [0, 0.25, 0.5, 0.75, 1]}),
        ),
        # scale to [0, 1] works if all 0
        (
            pl.DataFrame({"A": [0, 0, 0]}),
            (0, 1),
            pl.DataFrame({"A": [0, 0, 0]}),
        ),
        # empty dataframe returns empty
        (pl.DataFrame({"A": []}), (0, 1), pl.DataFrame({"A": []})),
        # works for single element
        (
            pl.DataFrame({"A": [1]}),
            (0, 1),
            pl.DataFrame({"A": [0]}),
        ),
        # works for repeated data
        (
            pl.DataFrame({"A": [2.0, 2.0, 2.0]}),
            (1, 2),
            pl.DataFrame({"A": [1.0, 1.0, 1.0]}),
        ),
        # scale to [0,1] works with negative values
        (
            pl.DataFrame({"A": [-1, 0, 1]}),
            (0, 1),
            pl.DataFrame({"A": [0, 0.5, 1]}),
        ),
        (
            pl.DataFrame({"A": [1.5, 2.5, 3.5]}),
            (0, 1),
            pl.DataFrame({"A": [0, 0.5, 1]}),
        ),
    ],
)
def test_min_max_scaler(input_data, feature_range, expected_output) -> None:
    assert min_max_scaler(input_data, feature_range).equals(expected_output)  # noqa: S101
