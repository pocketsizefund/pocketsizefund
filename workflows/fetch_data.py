import os
import subprocess
from io import BytesIO

import httpx
import polars as pl
from google.cloud import run_v2
from pyarrow import ipc
from union import task


@task
def get_identity_token() -> str:
    result = subprocess.run(  # noqa: S603
        ["gcloud", "auth", "print-identity-token"],  # noqa: S607
        capture_output=True,
        check=True,
        text=True,
    )
    return result.stdout.strip()


@task
def fetch_dates(start_date: str, end_date: str, token: str) -> pl.DataFrame | pl.Series:
    client = run_v2.ServicesClient()
    project = os.getenv("GCP_PROJECT", "fund-alpha")
    region = os.getenv("GCP_REGION", "us-east1")
    parent: str = f"projects/{project}/locations/{region}"

    datamanager_url: str | None = next(
        (
            service.uri
            for service in client.list_services(parent=parent)
            if "datamanager" in service.name
        ),
        None,
    )

    if datamanager_url is None:
        message = "Datamanager service not found in the specified project and region"
        raise ValueError(message)

    response: httpx.Response = httpx.get(
        f"{datamanager_url}/equity-bars",
        headers={"Authorization": f"Bearer {token}"},
        params={"start_date": start_date, "end_date": end_date},
        timeout=60,
    )
    response.raise_for_status()

    with BytesIO(response.content) as buf:
        reader = ipc.open_stream(buf)
        arrow_table = reader.read_all()
        return pl.from_arrow(arrow_table)
