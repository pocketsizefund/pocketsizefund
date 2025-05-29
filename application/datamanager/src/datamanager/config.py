import os
from pathlib import Path
import json
from functools import cached_property
from pydantic import BaseModel, Field, computed_field


class Polygon(BaseModel):
    api_key: str | None = Field(default=os.getenv("POLYGON_API_KEY"))
    base_url: str = "https://api.polygon.io"
    daily_bars: str = "/v2/aggs/grouped/locale/us/market/stocks/"


class Bucket(BaseModel):
    name: str | None = Field(default=os.getenv("DATA_BUCKET"))
    project: str | None = Field(default=os.getenv("GCP_PROJECT"))

    @computed_field
    def daily_bars_path(self) -> str:
        return f"gs://{self.name}/equity/bars/"


class GCP(BaseModel):
    bucket: Bucket = Bucket()
    credentials_path: str = os.getenv("GOOGLE_APPLICATION_CREDENTIALS", "")

    @cached_property
    def _creds(self) -> dict:
        with Path(self.credentials_path).open("r") as f:
            return json.load(f)

    @computed_field
    def key_id(self) -> str | None:
        return self._creds.get("client_email")

    @computed_field
    def secret(self) -> str:
        return json.dumps(self._creds).replace("'", "\\'")


class Settings(BaseModel):
    gcp: GCP = GCP()
    polygon: Polygon = Polygon()
