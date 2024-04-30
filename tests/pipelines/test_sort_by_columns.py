import polars as pl
import pytest

from pipelines.transformations import sort_by_columns


@pytest.mark.parametrize(
    "input_data, subset, descending, expected_output",
    [
        # Single column ascending
        (
            pl.DataFrame({"A": [3, 1, 2], "B": ["c", "a", "b"]}),
            ["A"],
            False,
            pl.DataFrame({"A": [1, 2, 3], "B": ["a", "b", "c"]}),
        ),
        # Single column descending
        (
            pl.DataFrame({"A": [3, 1, 2], "B": ["c", "a", "b"]}),
            ["A"],
            True,
            pl.DataFrame({"A": [3, 2, 1], "B": ["c", "b", "a"]}),
        ),
        # Multiple columns ascending
        (
            pl.DataFrame({"A": [1, 1, 2], "B": [3, 2, 3]}),
            ["A", "B"],
            False,
            pl.DataFrame({"A": [1, 1, 2], "B": [2, 3, 3]}),
        ),
        # Multiple columns mixed order (A asc, B desc)
        (
            pl.DataFrame({"A": [1, 1, 2], "B": [2, 3, 3]}),
            ["A", "B"],
            [False, True],
            pl.DataFrame({"A": [1, 1, 2], "B": [3, 2, 3]}),
        ),
        # Empty DataFrame
        (
            pl.DataFrame({"A": [], "B": []}),
            ["A"],
            False,
            pl.DataFrame({"A": [], "B": []}),
        ),
        # All identical values
        (
            pl.DataFrame({"A": [1, 1, 1], "B": ["x", "x", "x"]}),
            ["A", "B"],
            False,
            pl.DataFrame({"A": [1, 1, 1], "B": ["x", "x", "x"]}),
        ),
    ],
)
def test_sort_by_columns(input_data, subset, descending, expected_output):
    assert sort_by_columns(input_data, subset, descending).equals(expected_output)
