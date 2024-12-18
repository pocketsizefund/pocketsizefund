import requests
import json
import time

# ticker = "AXP"
tickers = [
    "AMGN",
    "AAPL",
    "BA",
    "CAT",
    "CVX",
    "CSCO",
    "KO",
    "DOW",
    "GS",
    "HD",
    "HON",
    "IBM",
    "INTC",
    "JNJ",
    "JPM",
    "MCD",
    "MRK",
    "MSFT",
    "NKE",
    "PG",
    "CRM",
    "TRV",
    "UNH",
    "VZ",
    "V",
    "WBA",
    "WMT",
    "DIS",
]

for ticker in tickers:
    url = f"https://data.alpaca.markets/v2/stocks/{ticker}/bars?timeframe=1D&start=2018-12-06T00%3A00%3A00Z&end=2024-12-06T00%3A00%3A00Z&adjustment=all&feed=sip&sort=asc"

    headers = {
        "accept": "application/json",
        "APCA-API-KEY-ID": "ADD KEY",
        "APCA-API-SECRET-KEY": "ADD SECRET",
    }

    response = requests.get(url, headers=headers)

    file_path = f"{ticker}.json"

    with open(file_path, "w") as file:
        json.dump(response.json(), file, indent=4)  # `indent=4` formats the JSON for readability

    print(f"JSON response saved to {file_path}")

    time.sleep(5)
