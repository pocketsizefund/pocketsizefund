from typing import Any, Dict
from datetime import datetime
import requests


class Client:
    def __init__(
        self,
        alpaca_api_key: str,
        alpaca_api_secret: str,
        is_paper: bool = True,
    ):
        self.alpaca_api_key = alpaca_api_key
        self.alpaca_api_secret = alpaca_api_secret
        self.is_paper = is_paper

    def get_account(self) -> Dict[str, Any]:
        if self.is_paper:
            prefix = "paper-"

        url = f"https://{prefix}api.alpaca.markets/v2/account"

        headers = {
            "accept": "application/json",
            "APCA-API-KEY-ID": self.alpaca_api_key,
            "APCA-API-SECRET-KEY": self.alpaca_api_secret,
        }

        response = requests.get(url, headers=headers)

        if response.status_code != 200:
            raise Exception(response.text)

        body = response.json()

        return {
            "timestamp": datetime.now().isoformat(),
            "cash_amount": float(body["cash"]),
            "long_market_value": float(body["long_market_value"]),
            "short_market_value": float(body["short_market_value"]),
        }
