import polars as pl
import pytest

from pipelines.transformations import drop_duplicates


@pytest.mark.parametrize(
    "input_data, subset, expected_output",
    [
        # drop on whole dataframe, no subsetting
        (
            pl.DataFrame({"A": [1, 1, 2], "B": [3, 3, 3]}),
            None,
            pl.DataFrame({"A": [1, 2], "B": [3, 3]}),
        ),
        # drop on single column subset
        (
            pl.DataFrame({"A": [1, 2, 2], "B": [3, 4, 4]}),
            ["A"],
            pl.DataFrame({"A": [1, 2], "B": [3, 4]}),
        ),
        # drop on single column subset
        (
            pl.DataFrame({"A": [1, 2, 2], "B": [2, 1, 1]}),
            ["B"],
            pl.DataFrame({"A": [1, 2], "B": [2, 1]}),
        ),
        # no duplicates returns same dataset
        (
            pl.DataFrame({"A": [1, 2, 3], "B": [3, 4, 5]}),
            None,
            pl.DataFrame({"A": [1, 2, 3], "B": [3, 4, 5]}),
        ),
        # all duplicates returns single value
        (pl.DataFrame({"A": [1, 1, 1]}), ["A"], pl.DataFrame({"A": [1]})),
        # empty returns empty
        (pl.DataFrame({"A": [], "B": []}), None, pl.DataFrame({"A": [], "B": []})),
        (
            pl.DataFrame({"A": [1, 1, 2, 2], "B": [1, 1, 2, 2]}),
            ["A", "B"],
            pl.DataFrame({"A": [1, 2], "B": [1, 2]}),
        ),
        (
            pl.DataFrame({"A": [1, 2, 3], "B": [3, 2, 1], "C": [1, 1, 1]}),
            ["C"],
            pl.DataFrame({"A": [1], "B": [3], "C": [1]}),
        ),
        (
            pl.DataFrame({"A": [1, 2, 3], "B": [2, 2, 2], "C": [1, 2, 3]}),
            ["B", "C"],
            pl.DataFrame({"A": [1, 2, 3], "B": [2, 2, 2], "C": [1, 2, 3]}),
        ),
        (
            pl.DataFrame({"A": [1, 2], "B": [1, 1], "C": [1, 1]}),
            ["B", "C"],
            pl.DataFrame({"A": [1], "B": [1], "C": [1]}),
        ),
    ],
)
def test_drop_duplicates(input_data, subset, expected_output) -> None:
    # the tests seem to randomize the row order
    result = drop_duplicates(input_data, subset)

    if expected_output.columns:
        expected_sorted = expected_output.sort(expected_output.columns)
        result_sorted = result.sort(expected_output.columns)
    else:
        result_sorted = result_output
        expected_sorted = expected_output

    assert result_sorted.equals(expected_sorted)
