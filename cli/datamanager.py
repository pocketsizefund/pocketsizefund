import argparse
import json
from datetime import datetime, timedelta
from urllib.parse import urlparse
from zoneinfo import ZoneInfo

import boto3
import requests
from botocore.auth import SigV4Auth
from botocore.awsrequest import AWSRequest
from loguru import logger


def sign_request(
    method: str,
    url: str,
    data: dict | None = None,
    region: str = "us-east-1",
) -> dict:
    session = boto3.Session()
    credentials = session.get_credentials()

    request_payload = {
        "method": method,
        "url": url,
        "headers": {
            "Content-Type": "application/json",
            "Host": urlparse(url).netloc,
        },
    }

    if data:
        request_payload["data"] = json.dumps(data)
        request_payload["headers"]["Content-Type"] = "application/json"

    request = AWSRequest(**request_payload)
    SigV4Auth(credentials, "execute-api", region).add_auth(request)

    return {
        "method": method,
        "url": url,
        "headers": dict(request.headers),
        "data": request.body,
    }


def get_health(api_url: str, region: str) -> dict:
    signed_request = sign_request(method="GET", url=f"{api_url}/health", region=region)
    response = requests.request(**signed_request, timeout=10)
    response.raise_for_status()
    return response.json()


def get_equity_bars(api_url: str, start_date: str, end_date: str, region: str) -> dict:
    url = f"{api_url}/equity-bars?start_date={start_date}&end_date={end_date}"
    signed_request = sign_request(method="GET", url=url, region=region)
    response = requests.request(**signed_request, timeout=30)
    response.raise_for_status()
    return response.json()


def get_metrics(api_url: str, region: str) -> dict:
    signed_request = sign_request(method="GET", url=f"{api_url}/metrics", region=region)
    response = requests.request(**signed_request, timeout=30)
    response.raise_for_status()
    return response.json()


def fetch_equity_bars(api_url: str, fetch_date: str, region: str) -> dict:
    data = {"date": fetch_date}
    signed_request = sign_request(
        method="POST",
        url=f"{api_url}/equity-bars/fetch",
        data=data,
        region=region,
    )
    response = requests.request(**signed_request, timeout=60)
    response.raise_for_status()
    return response.json()


def main() -> None:
    parser = argparse.ArgumentParser(description="PocketSizeFund CLI Example")
    parser.add_argument("--api-url", required=True, help="API Gateway URL")
    parser.add_argument("--region", default="us-east-1", help="AWS region")
    parser.add_argument(
        "--command", choices=["health", "bars", "metrics", "fetch"], default="health"
    )
    parser.add_argument("--start-date", help="Start date for bars (YYYY-MM-DD)")
    parser.add_argument("--end-date", help="End date for bars (YYYY-MM-DD)")
    parser.add_argument("--fetch-date", help="Date to fetch bars for (YYYY-MM-DD)")

    args = parser.parse_args()

    eastern_timezone = ZoneInfo("America/New_York")

    today = datetime.now(tz=eastern_timezone).date()
    try:
        if args.command == "health":
            result = get_health(args.api_url, args.region)
            logger.info(json.dumps(result, indent=2))

        elif args.command == "metrics":
            result = get_metrics(args.api_url, args.region)
            logger.info(json.dumps(result, indent=2))

        elif args.command == "bars":
            if not args.start_date or not args.end_date:
                # Default to last 7 days
                end_date = today
                start_date = end_date - timedelta(days=7)
                start_date_str = start_date.isoformat()
                end_date_str = end_date.isoformat()
            else:
                start_date_str = args.start_date
                end_date_str = args.end_date

            result = get_equity_bars(
                args.api_url, start_date_str, end_date_str, args.region
            )
            records = result.get("records", [])
            logger.info(f"found {len(records)} records")
            if records:
                logger.info(json.dumps(records[0], indent=2))

        elif args.command == "fetch":
            fetch_date = args.fetch_date or today.isoformat()
            result = fetch_equity_bars(args.api_url, fetch_date, args.region)
            logger.info(json.dumps(result, indent=2))

    except requests.exceptions.HTTPError as e:
        logger.error(f"http error response: {e.response.text}")
    except Exception as e:  # noqa: BLE001
        logger.error(f"error: {e}")


if __name__ == "__main__":
    main()
