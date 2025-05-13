import unittest
from unittest.mock import patch, MagicMock
from datetime import datetime
from application.datamanager.src.datamanager.main import Database


class TestDatabase(unittest.TestCase):
    @patch("application.datamanager.src.datamanager.main.Database.__init__")
    def test_query_without_gcp(self, mock_init):
        # Setup mock
        mock_init.return_value = None
        db = Database()
        mock_result = MagicMock()
        db.connection = MagicMock()
        db.connection.execute.return_value = mock_result
        db.gcp_gcs_bucket = ""

        # Call the method
        start_date = datetime(2025, 5, 1)
        end_date = datetime(2025, 5, 2)
        db.query(start_date, end_date)

        # Assertions
        db.connection.execute.assert_called_once_with("SELECT * FROM equity_bars;")
        mock_result.pl.assert_called_once()

    @patch("application.datamanager.src.datamanager.main.Database.__init__")
    def test_query_with_gcp(self, mock_init):
        # Setup mock
        mock_init.return_value = None
        db = Database()
        mock_result = MagicMock()
        db.connection = MagicMock()
        db.connection.execute.return_value = mock_result
        db.gcp_gcs_bucket = "test-bucket"

        # Call the method
        start_date = datetime(2025, 5, 1)
        end_date = datetime(2025, 5, 2)
        db.query(start_date, end_date)

        # Assertions
        # Verify that the GCP query includes the correct filepaths
        execute_call = db.connection.execute.call_args[0][0]
        self.assertIn("SELECT * FROM read_csv(", execute_call)
        self.assertIn("gs://test-bucket/equity/bars/2025-05-01/data.csv", execute_call)
        self.assertIn("gs://test-bucket/equity/bars/2025-05-02/data.csv", execute_call)
        mock_result.pl.assert_called_once()

    @patch("application.datamanager.src.datamanager.main.Database.__init__")
    def test_query_invalid_dates(self, mock_init):
        # Setup mock
        mock_init.return_value = None
        db = Database()

        # Call the method with invalid dates
        start_date = datetime(2025, 5, 2)
        end_date = datetime(2025, 5, 1)

        # Assertions
        with self.assertRaises(ValueError):
            db.query(start_date, end_date)


if __name__ == "__main__":
    unittest.main()
