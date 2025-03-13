import polars as pl
from pricemodel.dataset import DataSet


def test_dataset():
    data = pl.DataFrame(
        {
            "timestamp": [
                "2023-01-01",
                "2023-01-02",
                "2023-01-03",
                "2023-01-01",
                "2023-01-02",
                "2023-01-03",
            ],
            "open_price": [100, 101, 102, 50, 51, 52],
            "high_price": [110, 111, 112, 60, 61, 62],
            "low_price": [90, 91, 92, 40, 41, 42],
            "close_price": [105, 106, 107, 55, 56, 57],
            "volume": [1000, 1100, 1200, 500, 600, 700],
            "volume_weighted_average_price": [105, 106, 107, 55, 56, 57],
            "ticker": ["AAPL", "AAPL", "AAPL", "GOOGL", "GOOGL", "GOOGL"],
        }
    )

    dataset = DataSet(
        batch_size=2,
        sequence_length=3,
    )

    assert dataset.batch_size == 2, "Batch size should be 2"

    dataset.load_data(data)

    assert len(dataset.preprocessors) == 2, "There should be two preprocessors"
    assert dataset.data.shape == (9, 6), "Data shape should be (9, 6)"

    preprocessors = dataset.get_preprocessors()

    assert len(preprocessors) == 4, "There should be four preprocessors"

    for iteration in dataset:
        assert len(iteration) == 3, "Each iteration should yield three items"
