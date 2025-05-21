import os
from contextlib import asynccontextmanager
from fastapi import FastAPI, Request, Response, status
from alpaca.trading.client import TradingClient


@asynccontextmanager
async def lifespan(app: FastAPI):
    app.state.client = TradingClient(
        os.getenv("ALPACA_API_KEY"),
        os.getenv("ALPACA_API_SECRET"),
        paper=os.getenv("ALPACA_PAPER", "false").lower() == "true",
    )
    yield


application = FastAPI(lifespan=lifespan)


@application.get("/health")
async def get_health():
    return Response(status_code=status.HTTP_200_OK)


@application.get("/account")
async def get_account(request: Request):
    return request.app.state.client.get_account()
