from concurrent import futures
import io
import tarfile

import boto3
import pandas


PREFIX_EQUITY_BARS_RAW_PATH = 'equity/raw/bars'
# PREFIX_EQUITY_FILINGS_RAW_PATH = 'equity/raw/filings' # temporarily removed
PREFIX_EQUITY_BARS_FEATURES_PATH = 'equity/features/bars'


class Client:
    def __init__(
        self,
        s3_data_bucket_name: str,
        s3_artifacts_bucket_name: str,
    ) -> None:
        self.s3_data_bucket_name = s3_data_bucket_name
        self.s3_artifacts_bucket_name = s3_artifacts_bucket_name
        self.s3_client = boto3.client('s3')

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

            if response['KeyCount'] == 0:
                return file_names

            for content in response['Contents']:
                key = content['Key']
                file_name = key.rsplit('/', 1)[1]
                file_names.append(file_name)

            if not response['IsTruncated']:
                break

        return file_names

    def store_dataframes(
        self,
        prefix: str,
        dataframes_by_file_name: dict[str, pandas.DataFrame],
    ) -> None:
        return self._store_objects_by_file_name(
            prefix,
            dataframes_by_file_name,
            self._store_dataframe,
        )

    def _store_dataframe(
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
        file_names: list[str],
    ) -> dict[str, pandas.DataFrame]:
        return self._load_objects_by_file_name(
            prefix,
            file_names,
            self._load_dataframe,
        )

    def _load_dataframe(
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

    def store_texts(
        self,
        prefix: str,
        texts_by_file_name: dict[str, str],
    ) -> None:
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
            object = objects_by_file_name[file_name]

            key = '{}/{}'.format(prefix, file_name)

            executed_future = executor.submit(
                store_function,
                object,
                key,
            )
            executed_futures.append(executed_future)

            for executed_future in futures.as_completed(executed_futures):
                try:
                    _ = executed_future.result()
                except Exception as error:
                    raise error

    def load_texts(
        self,
        prefix: str,
        file_names: list[str],
    ) -> str:
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
        key = '{}/{}'.format(prefix, file_name)

        response = self.s3_client.get_object(
            Bucket=self.s3_data_bucket_name,
            Key=key,
        )

        text = response['Body'].read().decode('utf-8')

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
            try:
                result = executed_future.result()
                for file_name in result.keys():
                    objects_by_file_name[file_name] = result[file_name]
            except Exception as error:
                raise error

        return objects_by_file_name

    def download_model_artifacts(
        self,
        model_name: str,
    ) -> None:
        response = self.s3_client.get_object(
            Bucket=self.s3_artifacts_bucket_name,
            Key=model_name,
        )

        compressed_data = response['Body'].read()

        compressed_file = tarfile.open(
            fileobj=io.BytesIO(compressed_data),
            mode='r:gz',
        )

        compressed_file.extractall()
