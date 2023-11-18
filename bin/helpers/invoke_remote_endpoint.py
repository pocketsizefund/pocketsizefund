import datetime

from sagemaker import tensorflow

from pkg.config import config
from pkg.data import data
from pkg.trade import trade


samconfig_file = config.SAMConfig(
    'samconfig.toml',
    config.ENVIRONMENT_DEVELOPMENT,
)

data_client = data.Client(
    alpha_vantage_api_key=samconfig_file.get_parameter('AlphaVantageAPIKey'),
    alpaca_api_key=samconfig_file.get_parameter('AlpacaAPIKey'),
    alpaca_api_secret=samconfig_file.get_parameter('AlpacaAPISecret'),
)

trade_client = trade.Client(
    darqube_api_key=samconfig_file.get_parameter('DarqubeAPIKey'),
    alpaca_api_key=samconfig_file.get_parameter('AlpacaAPIKey'),
    alpaca_api_secret=samconfig_file.get_parameter('AlpacaAPISecret'),
)

predictor = tensorflow.TensorFlowPredictor(
    endpoint_name='pocketsizefund-lstm',
)

available_tickers = trade_client.get_available_tickers()

end_at = datetime.datetime.now()
start_at = end_at - datetime.timedelta(days=1)

equity_bars = data_client.get_range_equities_bars(
    tickers=available_tickers,
    start_at=start_at,
    end_at=end_at,
)

equity_bars['timestamp'] = equity_bars['timestamp'].astype(str)

prediction = predictor.predict(
    data=equity_bars.to_dict(orient='records'),
)

print('prediction:', prediction)
