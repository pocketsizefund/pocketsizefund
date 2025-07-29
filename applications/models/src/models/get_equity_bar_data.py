import argparse
import os
from datetime import date, datetime
from typing import TYPE_CHECKING, cast

import boto3
import pandas as pd
import pyarrow as pa
import pyarrow.csv as pc
import requests
from botocore.auth import SigV4Auth
from botocore.awsrequest import AWSRequest
from internal.equity_bar import EquityBar
from polygon import RESTClient

if TYPE_CHECKING:
    from polygon.rest.models.aggs import GroupedDailyAgg


def get_datamanager_equity_bar_data_as_csv(start_date: date, end_date: date) -> None:
    session = boto3.Session()
    credentials = session.get_credentials()

    datamanager_base_url = os.getenv("DATAMANAGER_BASE_URL")
    if not datamanager_base_url:
        message = "DATAMANAGER_BASE_URL environment variable is not set."
        raise ValueError(message)

    aws_request = AWSRequest(
        method="GET",
        url=datamanager_base_url
        + "/equity-bars?start_date={}&end_date={}".format(
            start_date.strftime("%Y-%m-%d"),
            end_date.strftime("%Y-%m-%d"),
        ),
        headers={"Content-Type": "application/json"},
    )

    SigV4Auth(credentials, "execute-api", os.getenv("AWS_REGION")).add_auth(aws_request)

    request = {
        "method": aws_request.method,
        "url": aws_request.url,
        "headers": dict(aws_request.headers),
        "data": aws_request.body,
    }

    response = requests.request(**request, timeout=30)

    response.raise_for_status()

    content_bytes = response.json()["content"]

    buffer = pa.BufferReader(content_bytes)
    reader = pa.ipc.open_stream(buffer)
    table = reader.read_all()

    pc.write_csv(
        table,
        "datamanager_equity_bars.csv",
        write_options=pc.WriteOptions(
            include_header=True,
        ),
    )


def get_polygon_equity_bar_data_as_csv(start_date: date, end_date: date) -> None:
    polygon_client = RESTClient(os.getenv("POLYGON_API_KEY"))

    all_daily_equity_bars: list[EquityBar] = []
    for request_date in pd.date_range(start_date, end_date):
        daily_equity_bars = polygon_client.get_grouped_daily_aggs(
            date=request_date.strftime("%Y-%m-%d"),
            adjusted=True,
        )

        daily_aggregates = cast("list[GroupedDailyAgg]", daily_equity_bars)

        daily_equity_bars = [
            EquityBar(
                ticker=cast("str", agg.ticker),
                timestamp=datetime.strptime(
                    cast("str", agg.timestamp),
                    "%Y%m%d %z",
                ).date(),
                open_price=cast("float", agg.open),
                high_price=cast("float", agg.high),
                low_price=cast("float", agg.low),
                close_price=cast("float", agg.close),
                volume=cast("int", agg.volume),
                volume_weighted_average_price=cast("float", agg.vwap),
            )
            for agg in daily_aggregates
        ]

        all_daily_equity_bars.append(*daily_equity_bars)

    equity_bars_dataframe = pd.DataFrame(all_daily_equity_bars)

    equity_bars_dataframe.to_csv(
        path_or_buf="polygon_equity_bars.csv",
        index=False,
    )


if __name__ == "__main__":
    parser = argparse.ArgumentParser(
        description="Fetch equity bar data from Polygon or application and save as CSV."
    )

    parser.add_argument(
        "--start-date",
        type=date.fromisoformat,
        required=True,
        help="Start date for fetching equity bar data (YYYY-MM-DD).",
    )

    parser.add_argument(
        "--end-date",
        type=date.fromisoformat,
        required=True,
        help="End date for fetching equity bar data (YYYY-MM-DD).",
    )

    parser.add_argument(
        "--source",
        choices=["datamanager", "polygon"],
        required=True,
        help="Source of equity bar data.",
    )

    arguments = parser.parse_args()

    if arguments.source == "datamanager":
        get_datamanager_equity_bar_data_as_csv(
            start_date=arguments.start_date,
            end_date=arguments.end_date,
        )

    elif arguments.source == "polygon":
        get_polygon_equity_bar_data_as_csv(
            start_date=arguments.start_date,
            end_date=arguments.end_date,
        )

    else:
        message = f"Unknown source: {arguments.source}"
        raise ValueError(message)
