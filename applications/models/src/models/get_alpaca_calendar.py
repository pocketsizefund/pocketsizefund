import os
from datetime import datetime, timedelta
from typing import cast
from zoneinfo import ZoneInfo

import polars as pl
from alpaca.trading import TradingClient
from alpaca.trading.models import Calendar
from alpaca.trading.requests import GetCalendarRequest
from loguru import logger

if __name__ == "__main__":
    api_key = os.getenv("ALPACA_API_KEY_ID")
    secret_key = os.getenv("ALPACA_API_SECRET_KEY")

    if not api_key or not secret_key:
        message = "Missing required environment variables: ALPACA_API_KEY_ID and/or ALPACA_API_SECRET_KEY"  # noqa: E501
        logger.error(message)
        raise ValueError(message)

    alpaca_client = TradingClient(
        api_key=api_key,
        secret_key=secret_key,
        paper=os.getenv("ALPACA_PAPER", "true").lower() == "true",
    )

    end = datetime.now(tz=ZoneInfo("America/New_York"))
    start = end - timedelta(days=365 * 6)

    try:
        calendars: list[Calendar] = cast(
            "list[Calendar]",
            alpaca_client.get_calendar(
                GetCalendarRequest(
                    start=start.date(),
                    end=end.date(),
                )
            ),
        )

    except Exception as e:
        logger.error(f"Error fetching Alpaca calendar: {e}")
        raise

    calendar_data: list[dict[str, str]] = [
        {
            "date": str(calendar.date),
            "open": str(calendar.open),
            "close": str(calendar.close),
        }
        for calendar in calendars
    ]

    calendar_content = pl.DataFrame(calendar_data)

    calendar_content.write_csv("calendar.csv")

    logger.info("Calendar data has been written to calendar.csv")
