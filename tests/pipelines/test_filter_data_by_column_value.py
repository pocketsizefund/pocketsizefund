import polars as pl
import pytest

from pipelines.transformations import filter_data_by_column_value


@pytest.mark.parametrize(
    "input_data, column, value, expected_output",
    [
        # Basic filtering
        (
            pl.DataFrame({"A": [1, 2, 3], "B": ["x", "y", "z"]}),
            "A",
            2,
            pl.DataFrame({"A": [2], "B": ["y"]}),
        ),
        # No matches
        (
            pl.DataFrame({"A": [1, 2, 3], "B": ["x", "y", "z"]}),
            "A",
            4,
            pl.DataFrame({"A": [], "B": []}),
        ),
        # Multiple matches
        (
            pl.DataFrame({"A": [1, 2, 2, 3], "B": ["x", "y", "y", "z"]}),
            "A",
            2,
            pl.DataFrame({"A": [2, 2], "B": ["y", "y"]}),
        ),
        # Case sensitivity
        (
            pl.DataFrame({"A": ["apple", "Apple", "APPLE"]}),
            "A",
            "Apple",
            pl.DataFrame({"A": ["Apple"]}),
        ),
        # Filtering on strings
        (
            pl.DataFrame({"A": ["apple", "banana", "cherry"]}),
            "A",
            "banana",
            pl.DataFrame({"A": ["banana"]}),
        ),
        # Empty DataFrame
        (pl.DataFrame({"A": []}), "A", "anything", pl.DataFrame({"A": []})),
    ],
)
def test_filter_data_by_column_value(
    input_data: pl.DataFrame, column: list[str], value: str | float, expected_output: pl.DataFrame
) -> None:
    assert filter_data_by_column_value(input_data, column, value).equals(expected_output)  # noqa: S101
