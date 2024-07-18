"""Contains the event-based publisher and subscriber resources."""

from kafka import KafkaConsumer, KafkaProducer


class Publisher:
    def __init__(self, bootstrap_server_url: str) -> None:
        """Initialize the Publisher class."""
        self.producer = KafkaProducer(
            bootstrap_servers=[bootstrap_server_url],
        )

    def send_event(self, topic_name: str, event: str) -> None:
        """Send the event to the specified topic."""
        self.producer.send(topic_name, value=event.encode("utf-8"))


class Subscriber:
    def __init__(self, bootstrap_server_url: str, topic_names: list[str]) -> None:
        """Initialize the Subscriber class."""
        self.consumer = KafkaConsumer(
            topic_names,
            bootstrap_servers=[bootstrap_server_url],
            value_deserializer=lambda x: x.decode("utf-8"),
        )

    def receive_events(self) -> list[str]:
        """Fetch events from the subscribed topics."""
        messages_by_topic_name = self.consumer.poll(timeout_ms=1000)

        return [
            message.value.decode("utf-8")
            for messages in messages_by_topic_name.values()
            for message in messages
        ]
