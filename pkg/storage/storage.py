from concurrent import futures
import gzip
import json
import io

import boto3
import pandas


PREFIX_EQUITY_BARS_RAW_PATH = 'equity/bars/raw/alphavantage'
PREFIX_EQUITY_BARS_PROCESSED_PATH = 'equity/bars/processed'


class Client:
    def __init__(
        self,
        s3_data_bucket_name: str,
    ) -> None:
        self.s3_data_bucket_name = s3_data_bucket_name
        self.s3_client = boto3.client('s3')

    def store_json_objects(
        self,
        path: str,
        json_objects: dict[any, any],
    ) -> None:
        executor = futures.ThreadPoolExecutor()

        executed_futures: list[futures.Future] = []
        for json_object in json_objects:
            if path == PREFIX_EQUITY_BARS_RAW_PATH:
                ticker = json_object['Meta Data']['2. Symbol']
                key = '{}/{}.json.gz'.format(path, ticker)

                executed_future = executor.submit(
                    self.__put_json_object,
                    json_object,
                    key,
                )
                executed_futures.append(executed_future)

                for executed_future in futures.as_completed(executed_futures):
                    try:
                        _ = executed_future.result()
                    except Exception as error:
                        raise error

    def __put_json_object(
        self,
        json_object: dict[any, any],
        key: str,
    ) -> None:
        json_string = json.dumps(json_object)
        json_bytes = json_string.encode('utf-8')
        json_compressed = gzip.compress(json_bytes)

        self.s3_client.put_object(
            Body=json_compressed,
            Bucket=self.s3_data_bucket_name,
            Key=key,
        )

    def list_file_names(
            self,
            prefix: str,
    ) -> list[str]:
        file_names: list[str] = []

        continuation_token: str = None
        while True:
            list_arguments = {
                'Bucket': self.s3_data_bucket_name,
                'Prefix': prefix,
            }

            if continuation_token:
                list_arguments['ContinuationToken'] = continuation_token

            response = self.s3_client.list_objects_v2(**list_arguments)

            for content in response['Contents']:
                key = content['Key']
                file_name = key.rsplit('/', 1)[1]
                file_names.append(file_name)

            if not response['IsTruncated']:
                break

        return file_names

    def load_json_objects(
        self,
        prefix: str,
        file_names: list[str] = [],
    ) -> dict[str, dict[any, any]]:
        executor = futures.ThreadPoolExecutor()

        executed_futures: list[futures.Future] = []
        for file_name in file_names:
            executed_future = executor.submit(
                self.__get_json_object,
                prefix,
                file_name,
            )
            executed_futures.append(executed_future)

            json_objects: dict[str, dict[any, any]] = {}
            for executed_future in futures.as_completed(executed_futures):
                try:
                    result = executed_future.result()
                    for key in result.keys():
                        json_objects[key] = result[key]
                except Exception as error:
                    raise error

        return json_objects

    def __get_json_object(
        self,
        prefix: str,
        file_name: str,
    ) -> dict[any, any]:
        key = '{}/{}'.format(prefix, file_name)
        response = self.s3_client.get_object(
            Bucket=self.s3_data_bucket_name,
            Key=key,
        )

        json_gzip = gzip.GzipFile(fileobj=response['Body'])
        json_bytes = json_gzip.read()
        json_string = json_bytes.decode('utf-8')
        json_object = json.loads(json_string)

        return {
            file_name: json_object,
        }

    def store_dataframes(
            self,
            prefix: str,
            dataframes: dict[str, pandas.DataFrame],
    ) -> None:
        executor = futures.ThreadPoolExecutor()

        executed_futures: list[futures.Future] = []
        for file_name in dataframes:
            dataframe = dataframes[file_name]

            key = '{}/{}'.format(prefix, file_name)

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
            compression='gzip',
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
        key = '{}/{}'.format(prefix, file_name)
        response = self.s3_client.get_object(
            Bucket=self.s3_data_bucket_name,
            Key=key,
        )

        dataframe = pandas.read_csv(
            response['Body'],
            compression='gzip',
        )

        if 'timestamp' in dataframe.columns:
            dataframe['timestamp'] = dataframe['timestamp'].apply(
                pandas.Timestamp,
            )

        return {file_name: dataframe}
