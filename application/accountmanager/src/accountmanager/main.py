from fastapi import FastAPI, HTTPException
from alpaca.trading.client import TradingClient
import os


application = FastAPI()


def get_client() -> TradingClient:
    api_key = os.getenv("ALPACA_API_KEY", "")
    api_secret = os.getenv("ALPACA_API_SECRET", "")
    paper = os.getenv("ALPACA_PAPER", "true").lower() == "true"
    if not api_key or not api_secret:
        raise ValueError("Alpaca API key and secret are required")
    return TradingClient(api_key, api_secret, paper=paper)


@application.get("/health")
async def get_health():
    return {"status": "healthy"}


@application.get("/account")
async def get_account():
    try:
        client = get_client()
        account = client.get_account()
        return account.model_dump()
    except Exception as e:
        raise HTTPException(status_code=500, detail=str(e))
