import datetime
from unittest.mock import patch
from zoneinfo import ZoneInfo

import pytest
from internal.dates import Date, DateRange
from pydantic import ValidationError


def test_date_default() -> None:
    with patch("internal.dates.datetime") as mock_datetime:
        mock_datetime.datetime.now.return_value = datetime.datetime(
            2023, 5, 15, 10, 30, 0, tzinfo=ZoneInfo("America/New_York")
        )
        mock_datetime.date = datetime.date

        date_instance = Date()

    assert date_instance.date == datetime.date(2023, 5, 15)


def test_date_explicit_date() -> None:
    test_date = datetime.date(2023, 1, 1)
    date_instance = Date(date=test_date)

    assert date_instance.date == test_date


def test_date_string_dash_format() -> None:
    date_instance = Date(date=datetime.date.fromisoformat("2023-03-15"))

    assert date_instance.date == datetime.date(2023, 3, 15)


def test_date_json_encoder() -> None:
    test_date = datetime.date(2023, 6, 10)
    date_instance = Date(date=test_date)

    json_dict = date_instance.model_dump(mode="json")
    assert json_dict["date"] == "2023/06/10"


def test_date_range_valid() -> None:
    start = datetime.date(2023, 1, 1)
    end = datetime.date(2023, 12, 31)

    date_range = DateRange(start=start, end=end)

    assert date_range.start == start
    assert date_range.end == end


def test_date_range_end_before_start() -> None:
    start = datetime.date(2023, 12, 31)
    end = datetime.date(2023, 1, 1)

    with pytest.raises(ValidationError) as exc_info:
        DateRange(start=start, end=end)

    assert "End date must be after start date" in str(exc_info.value)


def test_date_range_same_dates() -> None:
    same_date = datetime.date(2023, 6, 15)

    with pytest.raises(ValidationError) as exc_info:
        DateRange(start=same_date, end=same_date)

    assert "End date must be after start date" in str(exc_info.value)


def test_date_range_one_day_apart() -> None:
    start = datetime.date(2023, 6, 15)
    end = datetime.date(2023, 6, 16)

    date_range = DateRange(start=start, end=end)

    assert date_range.start == start
    assert date_range.end == end


def test_date_range_to_object() -> None:
    start = datetime.date(2023, 3, 1)
    end = datetime.date(2023, 3, 31)

    date_range = DateRange(start=start, end=end)
    result = date_range.to_object()

    expected = {
        "start_date": "2023-03-01",
        "end_date": "2023-03-31",
    }

    assert result == expected


def test_date_range_to_object_leap_year() -> None:
    start = datetime.date(2024, 2, 28)
    end = datetime.date(2024, 2, 29)

    date_range = DateRange(start=start, end=end)
    result = date_range.to_object()

    expected = {
        "start_date": "2024-02-28",
        "end_date": "2024-02-29",
    }

    assert result == expected
