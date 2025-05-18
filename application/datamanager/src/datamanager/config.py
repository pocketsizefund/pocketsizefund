import os
from pydantic import BaseModel, Field, computed_field


class Polygon(BaseModel):
    api_key: str = Field(default=os.getenv("POLYGON_API_KEY"))
    base_url: str = "https://api.polygon.io"
    daily_bars: str = "/v2/aggs/grouped/locale/us/market/stocks/"


class Bucket(BaseModel):
    name: str = Field(default=os.getenv("DATA_BUCKET"))

    def daily_bars_path(self, date):
        return f"gs://{self.name}/equity/bars/{date.strftime('%Y/%m/%d')}/data.parquet"


class Settings(BaseModel):
    bucket: Bucket = Bucket()
    polygon: Polygon = Polygon()
