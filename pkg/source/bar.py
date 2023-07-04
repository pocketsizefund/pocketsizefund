import datetime


SOURCE_ALPHA_VANTAGE = 'ALPHA_VANTAGE'
SOURCE_ALPACA = 'ALPACA'


class Bar:
    def __init__(
        self,
        timestamp: datetime.datetime,
        ticker: str,
        open_price: float,
        high_price: float,
        low_price: float,
        close_price: float,
        volume: float,
        source: str,
    ) -> None:
        if timestamp is None:
            raise ValueError('timestamp is required')

        if ticker is None:
            raise ValueError('ticker is required')

        if open_price is None:
            raise ValueError('open_price is required')

        if high_price is None:
            raise ValueError('high_price is required')

        if low_price is None:
            raise ValueError('low_price is required')

        if close_price is None:
            raise ValueError('close_price is required')

        if volume is None:
            raise ValueError('volume is required')

        if source is None:
            raise ValueError('source is required')

        if source not in [
            SOURCE_ALPHA_VANTAGE,
            SOURCE_ALPACA,
        ]:
            raise ValueError('source is invalid')

        self.timestamp = timestamp
        self.ticker = ticker.upper()
        self.open_price = open_price
        self.high_price = high_price
        self.low_price = low_price
        self.close_price = close_price
        self.volume = volume
        self.source = source
