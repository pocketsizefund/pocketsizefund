import subprocess
import httpx
import polars as pl
import pyarrow.ipc as ipc
from io import BytesIO

from google.cloud import run_v2
from union import task


def get_identity_token() -> str:
    result = subprocess.run(
        ["gcloud", "auth", "print-identity-token"],
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE,
        check=True,
        text=True,
    )
    return result.stdout.strip()


@task
def fetch_dates(start_date: str, end_date: str) -> pl.DataFrame | pl.Series:
    client = run_v2.ServicesClient()
    project = "fund-alpha"
    region = "us-east1"
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
        raise ValueError("Datamanager service not found in the specified project and region")

    token = get_identity_token()

    response: httpx.Response = httpx.get(
        f"{datamanager_url}/equity-bars",
        headers={"Authorization": f"Bearer {token}"},
        params={"start_date": start_date, "end_date": end_date},
        timeout=60,
    )

    with BytesIO(response.content) as buf:
        reader = ipc.open_stream(buf)
        arrow_table = reader.read_all()
        data: pl.DataFrame | pl.Series | None = pl.from_arrow(arrow_table)

    return data
