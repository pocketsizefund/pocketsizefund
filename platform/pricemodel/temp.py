import pandas as pd
import numpy as np
from datetime import datetime

# Load the CSV file
df = pd.read_csv("platform/pricemodel/consolidated_data.csv")


# Generate 30 consecutive weekdays starting from today
start_date = datetime.today()
timestamps = pd.bdate_range(start=start_date, periods=30).strftime("%Y-%m-%d").tolist()


# Ensure each ticker has 30 varied rows with sequential timestamps
def generate_ticker_data_fixed(df, ticker, timestamps):
    ticker_df = df[df["ticker"] == ticker]

    if not ticker_df.empty:
        generated_data = {}
        for col in df.columns:
            if col == "ticker":
                generated_data[col] = [ticker] * 30
            elif col == "timestamp":
                generated_data[col] = timestamps  # Assign generated weekdays
            elif ticker_df[col].dtype != "O":  # Numeric columns
                generated_data[col] = np.random.uniform(
                    ticker_df[col].min(), ticker_df[col].max(), 30
                ).round(2)
            else:
                generated_data[col] = ticker_df[col].sample(n=30, replace=True).values

        return pd.DataFrame(generated_data)


# Apply to each ticker
tickers = df["ticker"].unique()
randomized_df_fixed = pd.concat(
    [generate_ticker_data_fixed(df, ticker, timestamps) for ticker in tickers]
)

# Save new dataset
randomized_df_fixed.to_csv("platform/pricemodel/prediction_data.csv", index=False)
