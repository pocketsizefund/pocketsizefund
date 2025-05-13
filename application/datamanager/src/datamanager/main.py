from fastapi import FastAPI
import polars as pl
from datetime import datetime, timedelta
import os
from application.datamanager.src.datamanager.db.connection import DatabaseConnection
from application.datamanager.src.datamanager.db.sample_data import SampleDataProvider


class Database:
    def __init__(
        self,
        gcp_key_id: str = "",
        gcp_secret: str = "",
        gcp_gcs_bucket: str = "",
    ):
        self.gcp_gcs_bucket = gcp_gcs_bucket
        self.connection = DatabaseConnection.create_connection(gcp_key_id, gcp_secret)

        # Seed sample data if not using GCP
        if not gcp_key_id or not gcp_secret:
            SampleDataProvider.seed_equity_bars_data(self.connection)

    def query(
        self,
        start_date: datetime,
        end_date: datetime,
    ) -> pl.DataFrame:
        if start_date > end_date:
            raise ValueError("Start date must be earlier than end date.")

        query_statement = ""
        if self.gcp_gcs_bucket:
            filepaths = []
            current_date = start_date
            while current_date <= end_date:
                filepath = f"gs://{self.gcp_gcs_bucket}/equity/bars/{current_date.strftime('%Y-%m-%d')}/data.csv"
                filepaths.append(filepath)
                current_date += timedelta(days=1)

            query_statement = f"SELECT * FROM read_csv({filepaths})"

        else:
            query_statement = "SELECT * FROM equity_bars;"

        return self.connection.execute(query_statement).pl()


application = FastAPI()

gcp_key_id = os.getenv("GCP_KEY_ID")
gcp_secret = os.getenv("GCP_SECRET")
gcp_gcs_bucket = os.getenv("GCP_GCS_BUCKET")

database = Database(
    gcp_key_id=gcp_key_id,
    gcp_secret=gcp_secret,
    gcp_gcs_bucket=gcp_gcs_bucket,
)


@application.get("/health")
async def get_health():
    return {"status": "healthy"}


@application.get("/equity-bars")
async def get_equity_bars(start_date: datetime, end_date: datetime):
    results = database.query(start_date, end_date)

    return {
        "data": results.to_dicts(),
        "metadata": {
            "start_date": start_date,
            "end_date": end_date,
            "count": len(results),
        },
    }
