import unittest
from unittest.mock import patch, MagicMock
from application.datamanager.src.datamanager.db.sample_data import SampleDataProvider


class TestSampleDataProvider(unittest.TestCase):
    @patch("application.datamanager.src.datamanager.db.sample_data.pl.DataFrame")
    def test_seed_equity_bars_data(self, mock_dataframe):
        # Setup mocks
        mock_connection = MagicMock()
        mock_df_instance = MagicMock()
        mock_dataframe.return_value = mock_df_instance

        # Call the method
        SampleDataProvider.seed_equity_bars_data(mock_connection)

        # Assertions
        mock_connection.register.assert_called_once_with(
            "equity_bars_data", mock_df_instance
        )
        mock_connection.execute.assert_called_once_with(
            "CREATE TABLE equity_bars AS SELECT * FROM equity_bars_data"
        )

        # Verify DataFrame creation
        data_arg = mock_dataframe.call_args[0][0]
        self.assertIn("timestamp", data_arg)
        self.assertIn("ticker", data_arg)
        self.assertIn("open_price", data_arg)
        self.assertIn("high_price", data_arg)
        self.assertIn("low_price", data_arg)
        self.assertIn("close_price", data_arg)
        self.assertIn("trade_count", data_arg)
        self.assertIn("volume", data_arg)
        self.assertIn("volume_weighted_average_price", data_arg)

        # Verify sample data content
        self.assertEqual(len(data_arg["timestamp"]), 5)
        self.assertEqual(data_arg["ticker"], ["AAPL", "AAPL", "AAPL", "AAPL", "AAPL"])


if __name__ == "__main__":
    unittest.main()
