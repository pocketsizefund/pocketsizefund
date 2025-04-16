from fastapi import FastAPI
import duckdb
import polars as pl
from datetime import datetime, timedelta
import os


class Database:
    def __init__(
        self,
        gcp_key_id: str = "",
        gcp_secret: str = "",
        gcp_gcs_bucket: str = "",
    ):
        self.gcp_gcs_bucket = gcp_gcs_bucket
        self.connection = None
        if gcp_key_id and gcp_secret:
            connection = duckdb.connect()

            connection.execute("INSTALL httpfs;")
            connection.execute("LOAD httpfs;")

            connection.execute(f"""CREATE SECRET (
                TYPE gcs,
                KEY_ID '{gcp_key_id}',
                SECRET '{gcp_secret}'
            );""")

            self.connection = connection

        else:
            connection = duckdb.connect()

            data = {
                "timestamp": [
                    "2025-05-01T09:30:00Z",
                    "2025-05-02T09:30:00Z",
                    "2025-05-03T09:30:00Z",
                    "2025-05-04T09:30:00Z",
                    "2025-05-05T09:30:00Z",
                ],
                "ticker": ["AAPL", "AAPL", "AAPL", "AAPL", "AAPL"],
                "open_price": [150.00, 151.80, 152.10, 153.30, 154.60],
                "high_price": [152.50, 153.20, 154.00, 155.00, 156.30],
                "low_price": [149.00, 150.50, 151.00, 152.20, 153.40],
                "close_price": [151.75, 152.00, 153.25, 154.50, 155.75],
                "trade_count": [1000, 1200, 1100, 1300, 1400],
                "volume": [50000, 55000, 52000, 57000, 60000],
                "volume_weighted_average_price": [
                    151.25,
                    151.90,
                    152.60,
                    153.80,
                    155.00,
                ],
            }

            connection.register("equity_bars_data", pl.DataFrame(data))

            connection.execute(
                "CREATE TABLE equity_bars AS SELECT * FROM equity_bars_data"
            )

            self.connection = connection

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
async def get_equity_bars(
    start_date: datetime,
    end_date: datetime,
):
    results = database.query(start_date, end_date)

    return {
        "data": results.to_dicts(),
        "metadata": {
            "start_date": start_date,
            "end_date": end_date,
            "count": len(results),
        },
    }
