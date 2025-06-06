import unittest
from datetime import date

import pytest
from pydantic import ValidationError

from application.datamanager.src.datamanager.models import (
    BarsSummary,
    DateRange,
    SummaryDate,
)


class TestSummaryDate(unittest.TestCase):
    def test_summary_date_initialization_default(self) -> None:
        summary_date = SummaryDate()
        assert isinstance(summary_date.date, date)

    def test_summary_date_initialization_with_date(self) -> None:
        test_date = date(2023, 5, 15)
        summary_date = SummaryDate(date=test_date)
        assert summary_date.date == test_date

    def test_summary_date_string_parsing_iso_format(self) -> None:
        summary_date = SummaryDate(date="2023-5-15")  # type: ignore
        assert summary_date.date == date(2023, 5, 15)

    def test_summary_date_string_parsing_slash_format(self) -> None:
        summary_date = SummaryDate(date="2023/05/15")  # type: ignore
        assert summary_date.date == date(2023, 5, 15)

    def test_summary_date_invalid_format(self) -> None:
        with pytest.raises(ValidationError, match="Invalid date format"):
            SummaryDate(date="invalid-date")  # type: ignore

    def test_summary_date_invalid_date_values(self) -> None:
        with pytest.raises(ValidationError):
            SummaryDate(date="2023-13-01")  # type: ignore

    def test_summary_date_json_encoder(self) -> None:
        test_date = date(2023, 5, 15)
        summary_date = SummaryDate(date=test_date)
        json_data = summary_date.model_dump(mode="json")
        assert json_data["date"] == "2023/05/15"


class TestDateRange(unittest.TestCase):
    def test_date_range_valid(self) -> None:
        start_date = date(2023, 1, 1)
        end_date = date(2023, 12, 31)
        date_range = DateRange(start=start_date, end=end_date)

        assert date_range.start == start_date
        assert date_range.end == end_date

    def test_date_range_same_dates(self) -> None:
        same_date = date(2023, 5, 15)
        with pytest.raises(ValidationError, match="End date must be after start date"):
            DateRange(start=same_date, end=same_date)

    def test_date_range_end_before_start(self) -> None:
        start_date = date(2023, 12, 31)
        end_date = date(2023, 1, 1)
        with pytest.raises(ValidationError, match="End date must be after start date"):
            DateRange(start=start_date, end=end_date)

    def test_date_range_valid_one_day_apart(self) -> None:
        start_date = date(2023, 5, 15)
        end_date = date(2023, 5, 16)
        date_range = DateRange(start=start_date, end=end_date)

        assert date_range.start == start_date
        assert date_range.end == end_date


class TestBarsSummary(unittest.TestCase):
    def test_bars_summary_initialization(self) -> None:
        bars_summary = BarsSummary(date="2023-05-15", count=1500)

        assert bars_summary.date == "2023-05-15"
        assert bars_summary.count == 1500  # noqa: PLR2004

    def test_bars_summary_zero_count(self) -> None:
        bars_summary = BarsSummary(date="2023-05-15", count=0)

        assert bars_summary.date == "2023-05-15"
        assert bars_summary.count == 0

    def test_bars_summary_negative_count(self) -> None:
        bars_summary = BarsSummary(date="2023-05-15", count=-1)

        assert bars_summary.date == "2023-05-15"
        assert bars_summary.count == -1

    def test_bars_summary_json_serialization(self) -> None:
        bars_summary = BarsSummary(date="2023-05-15", count=1500)
        json_data = bars_summary.model_dump()

        assert json_data == {"date": "2023-05-15", "count": 1500}

    def test_bars_summary_from_dict(self) -> None:
        data = {"date": "2023-05-15", "count": 1500}
        bars_summary = BarsSummary.model_validate(data)

        assert bars_summary.date == "2023-05-15"
        assert bars_summary.count == 1500  # noqa: PLR2004


class TestModelIntegration(unittest.TestCase):
    def test_summary_date_to_bars_summary(self) -> None:
        summary_date = SummaryDate(date="2023-05-15")  # type: ignore
        bars_summary = BarsSummary(
            date=summary_date.date.strftime("%Y-%m-%d"), count=100
        )

        assert bars_summary.date == "2023-05-15"
        assert bars_summary.count == 100  # noqa: PLR2004

    def test_multiple_model_validation(self) -> None:
        summary_date = SummaryDate(date="2023-05-15")  # type: ignore
        date_range = DateRange(start=date(2023, 1, 1), end=date(2023, 12, 31))
        bars_summary = BarsSummary(date="2023-05-15", count=1000)

        assert summary_date.date == date(2023, 5, 15)
        assert date_range.start == date(2023, 1, 1)
        assert date_range.end == date(2023, 12, 31)
        assert bars_summary.count == 1000  # noqa: PLR2004


if __name__ == "__main__":
    unittest.main()
