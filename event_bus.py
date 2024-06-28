"""General event bus for all services."""
import asyncio
from typing import Literal

from aiokafka import AIOKafkaConsumer, AIOKafkaProducer
from aiokafka.helpers import create_ssl_context
from homer import config
from pydantic import BaseModel


class Topic(BaseModel):
    """Kafka topic."""

    environment: Literal["paper", "live"] = "paper"
    fund: Literal["pocketsizefund"] = "pocketsizefund"
    domain: Literal["trade", "portfolio", "ml"]
    event: str
    group_id: str | None

    @property
    def name(self) -> str:
        """Get the name of the topic, ensuring it conforms to the psf naming convention."""
        return f"{self.environment}.{self.fund}.{self.domain}"

def create_producer(event_loop: asyncio.AbstractEventLoop) -> AIOKafkaProducer:
    """Create an asynchronous Kafka producer."""
    return AIOKafkaProducer(
        loop=event_loop,
        bootstrap_servers=config.KAFKA_BOOTSTRAP_SERVER,
        sasl_mechanism=config.KAFKA_SASL_MECHANISM,
        security_protocol=config.KAFKA_SECURITY_PROTOCOL,
        sasl_plain_username=config.KAFKA_USERNAME,
        sasl_plain_password=config.KAFKA_PASSWORD,
        ssl_context=create_ssl_context(),
    )

def create_consumer(event_loop: asyncio.AbstractEventLoop,
                    topic: str, group_id: str) -> AIOKafkaConsumer:
    """Create an asynchronous Kafka consumer."""
    return AIOKafkaConsumer(
        topic,
        loop=event_loop,
        bootstrap_servers=config.KAFKA_BOOTSTRAP_SERVER,
        sasl_mechanism=config.KAFKA_SASL_MECHANISM,
        security_protocol=config.KAFKA_SECURITY_PROTOCOL,
        sasl_plain_username=config.KAFKA_USERNAME,
        sasl_plain_password=config.KAFKA_PASSWORD,
        group_id=group_id,
        auto_offset_reset="earliest",
        ssl_context=create_ssl_context(),
    )
