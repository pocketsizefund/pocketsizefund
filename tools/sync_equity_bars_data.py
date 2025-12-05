import json
import sys
import time
from datetime import UTC, datetime, timedelta

import requests
from loguru import logger


def validate_and_parse_dates(date_range_json: str) -> tuple[datetime, datetime]:
    try:
        date_range = json.loads(date_range_json)
    except json.JSONDecodeError as e:
        message = f"invalid JSON format: {e.msg}"
        raise ValueError(message) from e

    if "start_date" not in date_range or "end_date" not in date_range:
        message = "missing required field(s): start_date and/or end_date"
        raise ValueError(message)

    try:
        start_date = datetime.strptime(date_range["start_date"], "%Y-%m-%d").replace(
            tzinfo=UTC
        )
        end_date = datetime.strptime(date_range["end_date"], "%Y-%m-%d").replace(
            tzinfo=UTC
        )
    except ValueError as e:
        message = "invalid date format. Use YYYY-MM-DD"
        raise ValueError(message) from e

    current_date = datetime.now(tz=UTC).replace(
        hour=0, minute=0, second=0, microsecond=0
    )
    maximum_days = 365 * 2  # two year limit

    minimum_allowed_date = current_date - timedelta(days=maximum_days)

    start_date = min(start_date, minimum_allowed_date)
    end_date = min(end_date, current_date)

    return start_date, end_date


def sync_equity_bars_for_date(base_url: str, date: datetime) -> int:
    url = f"{base_url}/equity-bars"
    date_string = date.strftime("%Y-%m-%dT00:00:00Z")

    response = requests.post(
        url,
        json={"date": date_string},
        headers={"Content-Type": "application/json"},
        timeout=60,
    )

    return response.status_code


def main() -> None:
    """Sync equity bars data"""
    if len(sys.argv) != 3:  # noqa: PLR2004
        logger.error("expected two arguments (base_url, date_range_json)")
        sys.exit(1)

    base_url = sys.argv[1]
    date_range_json = sys.argv[2]

    if not base_url:
        logger.error("base_url cannot be empty")
        sys.exit(1)

    try:
        start_date, end_date = validate_and_parse_dates(date_range_json)
    except ValueError as e:
        logger.error(f"{e}")
        sys.exit(1)

    logger.info(
        f"backfilling equity bars from {start_date.date()} to {end_date.date()}"
    )
    logger.info(f"URL: {base_url}/equity-bars")

    current_date = start_date
    request_count = 0

    while current_date <= end_date:
        request_count += 1

        logger.info(
            f"[{request_count}] syncing date: {current_date.strftime('%Y-%m-%d')}"
        )

        try:
            status_code = sync_equity_bars_for_date(base_url, current_date)
            logger.info(f"status code: {status_code}")

            if status_code >= 400:  # noqa: PLR2004
                logger.error(f"request failed with status {status_code}")
        except requests.RequestException as e:
            logger.error(f"{e}")

        current_date += timedelta(days=1)

        if current_date <= end_date:
            logger.info("waiting 15 seconds before next request")
            time.sleep(15)  # Polygon rate limit

    logger.info(f"backfill complete: {request_count} requests processed")


if __name__ == "__main__":
    main()
