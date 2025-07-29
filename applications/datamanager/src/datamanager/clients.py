from typing import cast

import boto3
from polygon import RESTClient
from polygon.rest.models.aggs import GroupedDailyAgg


class PolygonClient:
    def __init__(self, polygon_api_key: str) -> None:
        self.polygon_client = RESTClient(api_key=polygon_api_key)

    def get_all_equity_bars(self, date: str) -> list[GroupedDailyAgg]:
        grouped = self.polygon_client.get_grouped_daily_aggs(
            date=date,
            adjusted=True,
        )

        return cast("list[GroupedDailyAgg]", grouped)


class S3Client:
    def __init__(self, data_bucket_name: str) -> None:
        self.s3_client = boto3.client("s3")
        self.data_bucket_name = data_bucket_name
        self.daily_equity_bars_path = f"s3://{self.data_bucket_name}/equity/bars/"

    def list_objects(self, prefix: str = "") -> list[str]:
        objects = []
        paginator = self.s3_client.get_paginator("list_objects_v2")

        for page in paginator.paginate(Bucket=self.data_bucket_name, Prefix=prefix):
            if "Contents" in page:
                objects.extend([obj["Key"] for obj in page["Contents"]])

        return objects

    def delete_objects(self, object_names: list[str]) -> None:
        if not object_names:
            return

        delete_requests = [{"Key": obj} for obj in object_names]
        self.s3_client.delete_objects(
            Bucket=self.data_bucket_name,
            Delete={"Objects": delete_requests},
        )
