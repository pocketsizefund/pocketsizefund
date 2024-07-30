"""Set positions based on portfolio position and model predictions."""

import asyncio
import os
import random

import requests
import sentry_sdk
from event_bus import Topic, create_consumer, create_producer
from loguru import logger
from pocketsizefund.trade import trade
from sentry_sdk.integrations.loguru import LoggingLevels, LoguruIntegration

sentry_loguru = LoguruIntegration(
    level=LoggingLevels.INFO.value,
    event_level=LoggingLevels.ERROR.value,
)

sentry_sdk.init(
    dsn=os.getenv("SENTRY_DSN"),
    integrations=[sentry_loguru],
    traces_sample_rate=1.0,
)

STATUS_CODE_OK = 200
POSITIONS_COUNT = 10


ENVIRONMENT = os.getenv("ENVIRONMENT")
PRICE_MODEL_URL = f"http://price-model.{ENVIRONMENT}.svc.cluster.local:8080"

logger.info(f"launching listener for {ENVIRONMENT}")
logger.info(f"price model url: {PRICE_MODEL_URL}")


response = requests.get(
    url=PRICE_MODEL_URL + "/health",
    timeout=30,
)

logger.info(f"price-model response: status={response.status_code}: {response.text}")


def get_predictions() -> dict[str, any]:
    """Set positions based on portfolio position and model predictions."""
    trade_client = trade.Client(
        darqube_api_key=os.getenv("DARQUBE_API_KEY"),
        alpaca_api_key=os.getenv("ALPACA_API_KEY"),
        alpaca_api_secret=os.getenv("ALPACA_API_SECRET"),
        alpha_vantage_api_key=os.getenv("ALPHA_VANTAGE_API_KEY"),
        is_paper=os.getenv("IS_PAPER"),
    )

    response = requests.get(
        url=PRICE_MODEL_URL + "/predictions",
        timeout=30,
    )

    if response.status_code != STATUS_CODE_OK:
        msg = f"error getting predictions: {response.text}"
        raise Exception(msg)  # noqa: TRY002

    predictions_by_ticker = response.json()

    logger.info(f"predictions_by_ticker: {predictions_by_ticker}")

    random_ticker = random.choice(list(predictions_by_ticker.get("tickers").keys()))  # noqa: S311

    logger.info(f"random_ticker: {random_ticker}")

    trade_client.baseline_buy(ticker=random_ticker)

    return None


async def listener(consumer, producer, output_topic) -> None:  # noqa: ANN001
    """Listen to the kafka topic and processes the messages."""
    while True:
        try:
            async for message in consumer:
                logger.info(f"Message received: {message}")
                get_predictions()
                await producer.send_and_wait(output_topic.name, b"test")
                logger.info("Processed message and sent result")
        except Exception as e:  # noqa: BLE001
            await producer.send_and_wait(
                output_topic.name.replace("success", "error"),
                b"BROKEN!",
            )
            logger.error(f"Error in listener: {e!s}")


async def main() -> None:  # noqa: D103
    loop = asyncio.get_event_loop()

    topic = Topic(domain="trade", event="psf.cron.submitted", group_id="psf.cron")

    output_topic = Topic(
        domain="trade",
        event="psf.positionmanager.success",
        group_id="psf.cron",
    )

    logger.info(f"Starting listener for {topic.name}")
    logger.info(f"Starting producer for {output_topic.name}")

    consumer = None
    producer = None

    try:
        consumer = await create_consumer(event_loop=loop, topic=topic)
        producer = await create_producer(event_loop=loop)

        await consumer.start()
        await producer.start()

        await listener(consumer, producer, output_topic)

    except Exception as e:  # noqa: BLE001
        logger.error(f"Error in main: {e}")
    finally:
        logger.info("Shutting down...")
        if consumer:
            await consumer.stop()
        if producer:
            await producer.stop()


if __name__ == "__main__":
    asyncio.run(main())

