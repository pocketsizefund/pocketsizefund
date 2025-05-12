import polars as pl


class SampleDataProvider:
    @staticmethod
    def seed_equity_bars_data(connection):
        data = {
            "timestamp": [
                "2025-05-01T09:30:00Z",
                "2025-05-02T09:30:00Z",
                "2025-05-03T09:30:00Z",
                "2025-05-04T09:30:00Z",
                "2025-05-05T09:30:00Z",
            ],
            "ticker": ["AAPL", "AAPL", "AAPL", "AAPL", "AAPL"],
            "open_price": [150.00, 151.80, 152.10, 153.30, 154.60],
            "high_price": [152.50, 153.20, 154.00, 155.00, 156.30],
            "low_price": [149.00, 150.50, 151.00, 152.20, 153.40],
            "close_price": [151.75, 152.00, 153.25, 154.50, 155.75],
            "trade_count": [1000, 1200, 1100, 1300, 1400],
            "volume": [50000, 55000, 52000, 57000, 60000],
            "volume_weighted_average_price": [
                151.25,
                151.90,
                152.60,
                153.80,
                155.00,
            ],
        }

        connection.register("equity_bars_data", pl.DataFrame(data))
        connection.execute("CREATE TABLE equity_bars AS SELECT * FROM equity_bars_data")
