from pydantic import BaseModel


class Settings(BaseModel):
    polygon_api_key: str | None = None
    polygon_base_url: str = "https://api.polygon.io"
    gcp_key_id: str | None = None
    gcp_secret: str | None = None
    gcp_gcs_bucket: str | None = None
