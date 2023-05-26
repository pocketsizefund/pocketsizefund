from concurrent import futures
import gzip
import io
import json

import boto3


EQUITY_BARS_RAW_PATH = 'equity/bars/raw/alphavantage'


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
            if path == EQUITY_BARS_RAW_PATH:
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
