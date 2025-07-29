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
    alpaca_client = TradingClient(
        api_key=os.getenv("ALPACA_API_KEY_ID"),
        secret_key=os.getenv("ALPACA_API_SECRET_KEY"),
        paper=os.getenv("ALPACA_PAPER", "true").lower() == "true",
    )

    end = datetime.now(tz=ZoneInfo("America/New_York"))
    start = end - timedelta(days=365 * 6)

    calendars: list[Calendar] = cast(
        "list[Calendar]",
        alpaca_client.get_calendar(
            GetCalendarRequest(
                start=start,
                end=end,
            )
        ),
    )

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
