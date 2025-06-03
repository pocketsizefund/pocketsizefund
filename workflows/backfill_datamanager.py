from datetime import date, timedelta

import httpx
from flytekit import task, workflow


@task(retries=3)
def backfill_single_date(base_url: str, day: date) -> int:
    response = httpx.post(f"{base_url}/equity-bars", json={"date": day.isoformat()})
    response.raise_for_status()
    return response.json().get("count", 0)


@workflow
def backfill_equity_bars(base_url: str, start_date: date, end_date: date) -> list[int]:
    results: list[int] = []
    current = start_date
    while current <= end_date:
        results.append(backfill_single_date(base_url=base_url, day=current))
        current += timedelta(days=1)
    return results
