import polars as pl
import pytest

from pipelines.transformations import drop_nulls

dataframe = pl.DataFrame(
    {
        "A": [1, None, 3, None, 5],
        "B": [None, 2, None, 4, 5],
        "C": ["x", "y", "z", None, "v"],
    },
)


@pytest.mark.parametrize(
    "input_data, subset, expected_output",
    [
        # no subset, consider whole dataframe
        (dataframe, None, pl.DataFrame({"A": [5], "B": [5], "C": "v"})),
        # subset a single column
        (
            dataframe,
            ["A"],
            pl.DataFrame({"A": [1, 3, 5], "B": [None, None, 5], "C": ["x", "z", "v"]}),
        ),
        # subset a different column
        (
            dataframe,
            ["B"],
            pl.DataFrame({"A": [None, None, 5], "B": [2, 4, 5], "C": ["y", None, "v"]}),
        ),
        # subset multiple columns
        (dataframe, ["A", "B"], pl.DataFrame({"A": [5], "B": [5], "C": ["v"]})),
        # subset a non-numeric column
        (
            dataframe,
            ["C"],
            pl.DataFrame(
                {
                    "A": [1, None, 3, 5],
                    "B": [None, 2, None, 5],
                    "C": ["x", "y", "z", "v"],
                },
            ),
        ),
        # all nulls is empty
        (pl.DataFrame({"A": [None, None, None]}), ["A"], pl.DataFrame({"A": []})),
        # no nulls is the same
        (pl.DataFrame({"A": [1, 2, 3]}), ["A"], pl.DataFrame({"A": [1, 2, 3]})),
        # empty is empty
        (pl.DataFrame({"A": []}), None, pl.DataFrame({"A": []})),
        (pl.DataFrame({"A": [None]}), None, pl.DataFrame({"A": []})),
        (
            pl.DataFrame({"A": [1, None], "B": [None, 2]}),
            ["A", "B"],
            pl.DataFrame({"A": [], "B": []}),
        ),
    ],
)
def test_drop_nulls(
    input_data: pl.DataFrame, subset: list[str], expected_output: pl.DataFrame,
) -> None:
    assert drop_nulls(input_data, subset).equals(expected_output)  # noqa: S101
