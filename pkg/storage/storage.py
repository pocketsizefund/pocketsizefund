import re
from concurrent import futures
import io
import tarfile

import boto3
import pandas


PREFIX_EQUITY_BARS_PATH = "equity/bars"
PREFIX_FEATURES_PATH = "features"


class Client:
    def __init__(
        self,
        s3_data_bucket_name: str,
        s3_artifacts_bucket_name: str,
    ) -> None:
        self.s3_data_bucket_name = s3_data_bucket_name
        self.s3_artifacts_bucket_name = s3_artifacts_bucket_name
        self.s3_client = boto3.client("s3")

    def list_file_names(
        self,
        prefix: str,
    ) -> list[str]:
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

    def get_next_prefix_version(
        self,
        prefixes: list[str],
    ) -> str:
        if len(prefixes) == 0:
            return "v0"

        next_version_integer = self._get_max_version_integer(prefixes) + 1

        return "v{}".format(next_version_integer)

    def get_max_prefix_version(
        self,
        prefixes: list[str],
    ) -> str:
        if len(prefixes) == 0:
            return ""

        return "v{}".format(self._get_max_version_integer(prefixes))

    def _get_max_version_integer(
        self,
        prefixes: list[str],
    ) -> int:
        pattern = r"v(\d+)"

        version_integers = [
            int(re.search(pattern, prefix).group(1))
            for prefix in prefixes
            if re.search(pattern, prefix)
        ]

        return max(version_integers)

    def store_dataframes(
        self,
        prefix: str,
        dataframes: dict[str, pandas.DataFrame],
    ) -> None:
        executor = futures.ThreadPoolExecutor()

        executed_futures: list[futures.Future] = []
        for file_name in dataframes:
            dataframe = dataframes[file_name]

            key = "{}/{}".format(prefix, file_name)

            executed_future = executor.submit(
                self.__put_dataframe,
                dataframe,
                key,
            )
            executed_futures.append(executed_future)

        for executed_future in futures.as_completed(executed_futures):
            try:
                _ = executed_future.result()
            except Exception as error:
                raise error

    def __put_dataframe(
        self,
        dataframe: pandas.DataFrame,
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
        file_names: list[str] = [],
    ) -> dict[str, pandas.DataFrame]:
        dataframes: dict[str, pandas.DataFrame] = {}

        executor = futures.ThreadPoolExecutor()

        executed_futures: list[futures.Future] = []
        for file_name in file_names:
            executed_future = executor.submit(
                self.__get_dataframe,
                prefix,
                file_name,
            )
            executed_futures.append(executed_future)

        dataframes: dict[str, pandas.DataFrame] = {}
        for executed_future in futures.as_completed(executed_futures):
            try:
                result = executed_future.result()
                for key in result.keys():
                    dataframes[key] = result[key]
            except Exception as error:
                raise error

        return dataframes

    def __get_dataframe(
        self,
        prefix: str,
        file_name: str,
    ) -> pandas.DataFrame:
        key = "{}/{}".format(prefix, file_name)
        response = self.s3_client.get_object(
            Bucket=self.s3_data_bucket_name,
            Key=key,
        )

        dataframe = pandas.read_csv(
            response["Body"],
            compression="gzip",
        )

        if "timestamp" in dataframe.columns:
            dataframe["timestamp"] = dataframe["timestamp"].apply(
                pandas.Timestamp,
            )

        return {file_name: dataframe}

    def download_model_artifacts(
        self,
        key: str,
    ) -> None:
        response = self.s3_client.get_object(
            Bucket=self.s3_artifacts_bucket_name,
            Key=key,
        )

        compressed_data = response["Body"].read()

        compressed_file = tarfile.open(
            fileobj=io.BytesIO(compressed_data),
            mode="r:gz",
        )

        compressed_file.extractall()
