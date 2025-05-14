import unittest
from unittest.mock import patch, MagicMock
from application.datamanager.src.datamanager.db.connection import DatabaseConnection


class TestDatabaseConnection(unittest.TestCase):
    @patch("application.datamanager.src.datamanager.db.connection.duckdb")
    def test_create_connection_without_gcp(self, mock_duckdb):
        mock_connection = MagicMock()
        mock_duckdb.connect.return_value = mock_connection

        connection = DatabaseConnection.create_connection()

        mock_duckdb.connect.assert_called_once()
        self.assertEqual(connection, mock_connection)
        mock_connection.execute.assert_not_called()  # No GCP setup should be called

    @patch("application.datamanager.src.datamanager.db.connection.duckdb")
    def test_create_connection_with_gcp(self, mock_duckdb):
        mock_connection = MagicMock()
        mock_duckdb.connect.return_value = mock_connection

        connection = DatabaseConnection.create_connection("test_key_id", "test_secret")

        mock_duckdb.connect.assert_called_once()
        self.assertEqual(connection, mock_connection)

        self.assertEqual(mock_connection.execute.call_count, 3)
        mock_connection.execute.assert_any_call("INSTALL httpfs;")
        mock_connection.execute.assert_any_call("LOAD httpfs;")

        create_secret_call = mock_connection.execute.call_args_list[2]
        create_secret_arg = create_secret_call[0][0]
        self.assertIn("TYPE gcs", create_secret_arg)
        self.assertIn("KEY_ID 'test_key_id'", create_secret_arg)
        self.assertIn("SECRET 'test_secret'", create_secret_arg)


if __name__ == "__main__":
    unittest.main()
