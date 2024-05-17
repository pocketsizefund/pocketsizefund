"""Unit tests for select_columns function."""
import polars as pl
import pytest

from pipelines.transformations import select_columns


@pytest.mark.parametrize(
    "input_data, subset, expected_output",
    [
        # select 2 out of 3 columns
        (
            pl.DataFrame({"A": [1, 2, 3], "B": [4, 5, 6], "C": [7, 8, 9]}),
            ["A", "B"],
            pl.DataFrame({"A": [1, 2, 3], "B": [4, 5, 6]}),
        ),
        (
            pl.DataFrame({"A": [1, 2, 3], "B": [4, 5, 6]}),
            ["A"],
            pl.DataFrame({"A": [1, 2, 3]}),
        ),
        (pl.DataFrame({"A": [1, 2, 3]}), ["A"], pl.DataFrame({"A": [1, 2, 3]})),
        (
            pl.DataFrame({"A": [1, 2, 3], "B": [4, 5, 6], "C": [7, 8, 9]}),
            ["C"],
            pl.DataFrame({"C": [7, 8, 9]}),
        ),
        (pl.DataFrame({"A": [1, 2, 3]}), None, pl.DataFrame({"A": [1, 2, 3]})),
        (
            pl.DataFrame({"A": [1, 2, 3], "B": [4, 5, 6]}),
            ["B"],
            pl.DataFrame({"B": [4, 5, 6]}),
        ),
        (
            pl.DataFrame({"A": [1], "B": [2], "C": [3]}),
            ["A", "B", "C"],
            pl.DataFrame({"A": [1], "B": [2], "C": [3]}),
        ),
        (pl.DataFrame({"A": [], "B": []}), ["A"], pl.DataFrame({"A": []})),
        (
            pl.DataFrame({"A": [1, 2, 3], "B": [4, 5, 6], "C": [7, 8, 9]}),
            ["A", "C"],
            pl.DataFrame({"A": [1, 2, 3], "C": [7, 8, 9]}),
        ),
        (
            pl.DataFrame({"A": [1, 2, 3], "B": [4, 5, 6], "C": [7, 8, 9]}),
            ["B"],
            pl.DataFrame({"B": [4, 5, 6]}),
        ),
    ],
)
def test_select_columns(
    input_data: pl.DataFrame, subset: list[str], expected_output: pl.DataFrame,
) -> None:
    """Test select_columns function."""
    result = select_columns(input_data, subset)
    if isinstance(result, list):
        assert result == expected_output  # noqa: S101
    else:
        assert result.equals(pl.DataFrame(expected_output))  # noqa: S101
