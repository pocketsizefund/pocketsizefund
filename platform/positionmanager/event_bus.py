"""General event bus for all services."""
import asyncio
import os
from typing import Literal

from aiokafka import AIOKafkaConsumer, AIOKafkaProducer
from aiokafka.helpers import create_ssl_context
from loguru import logger
from pydantic import BaseModel

KAFKA_BOOTSTRAP_SERVER = os.getenv("KAFKA_BOOTSTRAP_SERVER")
KAFKA_SASL_MECHANISM = os.getenv("KAFKA_SASL_MECHANISM")
KAFKA_SECURITY_PROTOCOL = os.getenv("KAFKA_SECURITY_PROTOCOL")
KAFKA_USERNAME = os.getenv("KAFKA_USERNAME")
KAFKA_PASSWORD = os.getenv("KAFKA_PASSWORD")

logger.info(f"KAFKA_BOOTSTRAP_SERVER: {KAFKA_BOOTSTRAP_SERVER}")
logger.info(f"KAFKA_SASL_MECHANISM: {KAFKA_SASL_MECHANISM}")
logger.info(f"KAFKA_SECURITY_PROTOCOL: {KAFKA_SECURITY_PROTOCOL}")


class Topic(BaseModel):
    """Kafka topic."""

    domain: Literal["trade", "portfolio", "ml"]
    event: str
    group_id: str | None

    @property
    def name(self) -> str:
        """Get the name of the topic, ensuring it conforms to the psf naming convention."""
        return f"{self.domain}.{self.event}"

def create_producer(event_loop: asyncio.AbstractEventLoop) -> AIOKafkaProducer:
    """Create an asynchronous Kafka producer."""
    return AIOKafkaProducer(
        loop=event_loop,
        bootstrap_servers=KAFKA_BOOTSTRAP_SERVER,
        sasl_mechanism=KAFKA_SASL_MECHANISM,
        security_protocol=KAFKA_SECURITY_PROTOCOL,
        sasl_plain_username=KAFKA_USERNAME,
        sasl_plain_password=KAFKA_PASSWORD,
        ssl_context=create_ssl_context(),
    )

def create_consumer(event_loop: asyncio.AbstractEventLoop,
                    topic: Topic) -> AIOKafkaConsumer:
    """Create an asynchronous Kafka consumer."""
    return AIOKafkaConsumer(
        topic.name,
        loop=event_loop,
        bootstrap_servers=KAFKA_BOOTSTRAP_SERVER,
        sasl_mechanism=KAFKA_SASL_MECHANISM,
        security_protocol=KAFKA_SECURITY_PROTOCOL,
        sasl_plain_username=KAFKA_USERNAME,
        sasl_plain_password=KAFKA_PASSWORD,
        group_id=topic.group_id,
        auto_offset_reset="earliest",
        ssl_context=create_ssl_context(),
    )



