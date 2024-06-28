"""Interact with cloud storage."""

import io
from concurrent import futures

import boto3
import pandas as pd

PREFIX_EQUITY_BARS_RAW_PATH = "equity/raw/bars"


class Client:
    def __init__(
        self,
        s3_data_bucket_name: str,
        s3_artifacts_bucket_name: str,
    ) -> None:
        """Initialize a new instance of the `Client` class.

        This is used to interact with AWS S3 buckets.

        Args:
            s3_data_bucket_name (str): The name of the S3 data bucket.
            s3_artifacts_bucket_name (str): The name of the S3 artifacts bucket.

        Returns:
            None
        """
        self.s3_data_bucket_name = s3_data_bucket_name
        self.s3_artifacts_bucket_name = s3_artifacts_bucket_name
        self.s3_client = boto3.client("s3")

    def list_file_names(
        self,
        prefix: str,
    ) -> list[str]:
        """Retrieve a list of file names from the S3 data bucket that match the given prefix.

        Args:
            prefix (str): The prefix to filter the file names by.

        Returns:
            list[str]: A list of file names that match the given prefix.

        Raises:
            None
        """
        file_names: list[str] = []

        continuation_token: str = None
        while True:
            list_arguments = {
                "Bucket": self.s3_data_bucket_name,
                "Prefix": prefix,
            }

            if continuation_token:
                list_arguments["ContinuationToken"] = continuation_token

            response = self.s3_client.list_objects_v2(**list_arguments)

            if response["KeyCount"] == 0:
                return file_names

            for content in response["Contents"]:
                key = content["Key"]
                file_name = key.rsplit("/", 1)[1]
                file_names.append(file_name)

            if not response["IsTruncated"]:
                break

        return file_names

    def store_dataframes(
        self,
        prefix: str,
        dataframes_by_file_name: dict[str, pd.DataFrame],
    ) -> None:
        """Store multiple pandas DataFrames in the S3 data bucket with the given prefix.

        Args:
            prefix (str): The prefix to use for the stored dataframes.
            dataframes_by_file_name (dict[str, pd.DataFrame]): A dictionary mapping file names
                to pandas DataFrames to store.

        Returns:
            None
        """
        return self._store_objects_by_file_name(
            prefix,
            dataframes_by_file_name,
            self._store_dataframe,
        )

    def _store_dataframe(
        self,
        dataframe: pd.DataFrame,
        key: str,
    ) -> None:
        gzip_buffer = io.BytesIO()

        dataframe.to_csv(
            gzip_buffer,
            index=False,
            header=True,
            compression="gzip",
        )

        gzip_buffer.seek(0)

        self.s3_client.put_object(
            Body=gzip_buffer.getvalue(),
            Bucket=self.s3_data_bucket_name,
            Key=key,
        )

    def load_dataframes(
        self,
        prefix: str,
        file_names: list[str],
    ) -> dict[str, pd.DataFrame]:
        """Load multiple pandas DataFrames from the S3 data bucket with the given prefix.

        Args:
            prefix (str): The prefix to use for the loaded dataframes.
            file_names (list[str]): A list of file names to load.

        Returns:
            dict[str, pd.DataFrame]: A dictionary mapping file names to loaded pandas DataFrames.
        """
        return self._load_objects_by_file_name(
            prefix,
            file_names,
            self._load_dataframe,
        )

    def _load_dataframe(
        self,
        prefix: str,
        file_name: str,
    ) -> pd.DataFrame:
        key = f"{prefix}/{file_name}"

        response = self.s3_client.get_object(
            Bucket=self.s3_data_bucket_name,
            Key=key,
        )

        dataframe = pd.read_csv(
            response["Body"],
            compression="gzip",
        )

        if "timestamp" in dataframe.columns:
            dataframe["timestamp"] = dataframe["timestamp"].apply(
                pd.Timestamp,
            )

        return {file_name: dataframe}

    def store_texts(
        self,
        prefix: str,
        texts_by_file_name: dict[str, str],
    ) -> None:
        """Store multiple text files in the S3 data bucket with the given prefix.

        Args:
            prefix (str): The prefix to use for the stored text files.
            texts_by_file_name (dict[str, str]): A dictionary mapping file names
                to text content to store.

        Returns:
            None
        """
        return self._store_objects_by_file_name(
            prefix,
            texts_by_file_name,
            self._store_text,
        )

    def _store_text(
        self,
        text: str,
        key: str,
    ) -> None:
        self.s3_client.put_object(
            Body=text,
            Bucket=self.s3_data_bucket_name,
            Key=key,
        )

    def _store_objects_by_file_name(
        self,
        prefix: str,
        objects_by_file_name: dict[str, any],
        store_function: any,
    ) -> None:
        executor = futures.ThreadPoolExecutor()

        executed_futures: list[futures.Future] = []
        for file_name in objects_by_file_name:
            object_to_store = objects_by_file_name[file_name]

            key = f"{prefix}/{file_name}"

            executed_future = executor.submit(
                store_function,
                object_to_store,
                key,
            )
            executed_futures.append(executed_future)

            for executed_future in futures.as_completed(executed_futures):
                _ = executed_future.result()

    def load_texts(
        self,
        prefix: str,
        file_names: list[str],
    ) -> str:
        """Load multiple text files from the S3 data bucket with the given prefix.

        Args:
            prefix (str): The prefix to use for the loaded text files.
            file_names (list[str]): A list of file names to load.

        Returns:
            str: The loaded text content concatenated into a single string.
        """
        return self._load_objects_by_file_name(
            prefix,
            file_names,
            self._load_text,
        )

    def _load_text(
        self,
        prefix: str,
        file_name: str,
    ) -> str:
        key = f"{prefix}/{file_name}"

        response = self.s3_client.get_object(
            Bucket=self.s3_data_bucket_name,
            Key=key,
        )

        text = response["Body"].read().decode("utf-8")

        return {file_name: text}

    def _load_objects_by_file_name(
        self,
        prefix: str,
        file_names: list[str],
        load_function: any,
    ) -> dict[str, any]:
        objects_by_file_name: dict[str, any] = {}

        executor = futures.ThreadPoolExecutor()

        executed_futures: list[futures.Future] = []
        for file_name in file_names:
            executed_future = executor.submit(
                load_function,
                prefix,
                file_name,
            )
            executed_futures.append(executed_future)

        for executed_future in futures.as_completed(executed_futures):
            result = executed_future.result()
            for file_name in result:
                objects_by_file_name[file_name] = result[file_name]

        return objects_by_file_name
