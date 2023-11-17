import datetime

import requests

from pkg.data import data
from pkg.trade import trade


data_client = data.Client(
    alpha_vantage_api_key='RUQBWWBHF3CWCTTE',
    alpaca_api_key='PKLEO0GN87N4GONALF13',
    alpaca_api_secret='vEh6wej2lwtUx2EhtuTBnFuq4z8Wbgtn8LNMftCK',
)

trade_client = trade.Client(
    darqube_api_key='6262242baa0d4447918fdf5b4a64880e',
    alpaca_api_key='PKLEO0GN87N4GONALF13',
    alpaca_api_secret='vEh6wej2lwtUx2EhtuTBnFuq4z8Wbgtn8LNMftCK',
)

available_tickers = trade_client.get_available_tickers()

start_at = datetime.datetime(2023, 11, 9)
end_at = datetime.datetime(2023, 11, 10)

equity_bars = data_client.get_range_equities_bars(
    tickers=available_tickers,
    start_at=start_at,
    end_at=end_at,
)

equity_bars['timestamp'] = equity_bars['timestamp'].astype(str)

response = requests.post(
    url='http://localhost:8080/invocations',
    json=equity_bars.to_dict(orient='records'),
    headers={'Content-Type': 'application/json'},
)

print('response:', response)
