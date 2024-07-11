"""Set positions based on portfolio position and model predictions."""

import asyncio
import datetime
import os

import requests
import sentry_sdk
from event_bus import Topic, create_consumer, create_producer
from loguru import logger
from pocketsizefund import config, trade
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


@config.api_key_required
def get_predictions() -> dict[str, any]:
    """Set positions based on portfolio position and model predictions."""
    trade_client = trade.Client(
        darqube_api_key=os.getenv("DARQUBE_API_KEY"),
        alpaca_api_key=os.getenv("ALPACA_API_KEY"),
        alpaca_api_secret=os.getenv("ALPACA_API_SECRET"),
        alpha_vantage_api_key=os.getenv("ALPHA_VANTAGE_API_KEY"),
        is_paper=os.getenv("IS_PAPER") == "true",
    )

    now = datetime.datetime.now(tz=config.TIMEZONE)

    is_clear = trade_client.check_set_position_availability(
        action=trade.CLEAR_ACTION,
        current_datetime=now,
    )

    is_create = trade_client.check_set_position_availability(
        action=trade.CREATE_ACTION,
        current_datetime=now,
    )

    if is_clear:
        trade_client.clear_positions()

    if is_create:
        response = requests.get(
            url="http://price-model:8080/predictions",
            timeout=30,
        )

        if response.status_code != STATUS_CODE_OK:
            msg = f"error getting predictions: {response.text}"
            raise Exception(msg)  # noqa: TRY002

        predictions_by_ticker = response.json()

        moves_by_ticker = {
            ticker: predictions_by_ticker[ticker][0] - predictions_by_ticker[ticker][4]
            for ticker in predictions_by_ticker
        }

        sorted_moves_by_ticker = dict(
            sorted(
                moves_by_ticker.items(),
                key=lambda item: item[1],
                reverse=True,
            ),
        )

        highest_moves_by_ticker = {
            k: sorted_moves_by_ticker[k] for k in list(sorted_moves_by_ticker)[:POSITIONS_COUNT]
        }

        highest_moves_tickers = highest_moves_by_ticker.keys()

        if len(highest_moves_tickers) == 0:
            msg = "no tickers to trade"
            raise Exception(msg)  # noqa: TRY002

        trade_client.set_positions(tickers=highest_moves_tickers)

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
            await producer.send_and_wait(output_topic.name.replace("success", "error"), b"BROKEN!")
            logger.error(f"Error in listener: {e!s}")


async def main() -> None:  # noqa: D103
    loop = asyncio.get_event_loop()

    topic = Topic(domain="trade", event="psf.cron.submitted", group_id="psf.cron")
    output_topic = Topic(domain="trade", event="psf.positionmanager.success", group_id="psf.cron")

    logger.info(f"Starting listener for {topic.name}")
    logger.info(f"Starting producer for {output_topic.name}")

    consumer = None
    producer = None

    try:
        consumer = create_consumer(event_loop=loop, topic=topic)
        producer = create_producer(event_loop=loop)

        await consumer.start()
        await producer.start()

        stop_event = asyncio.Event()

        await stop_event.wait()

    except Exception as e:  # noqa: BLE001
        logger.error(f"Error in main: {e!s}")
    finally:
        logger.info("Shutting down...")
        if consumer:
            await consumer.stop()
        if producer:
            await producer.stop()


if __name__ == "__main__":
    asyncio.run(main())
