from unittest.mock import Mock, patch

from pocketsizefund.event import event


@patch("pocketsizefund.event.event.KafkaProducer")
def test_publisher_send_event_success(mock_producer: any) -> None:
    mock_producer_instance = Mock()
    mock_producer.return_value = mock_producer_instance

    publisher = event.Publisher(bootstrap_server_url="bootstrap_server_url")

    publisher.send_event(
        topic_name="test-topic-name",
        event="test-event",
    )

    mock_producer_instance.send.assert_called_once_with(
        "test-topic-name",
        value=b"test-event",
    )


@patch("pocketsizefund.event.event.KafkaConsumer")
def test_subscriber_retrieve_events_success(mock_consumer: any) -> None:
    mock_consumer_instance = Mock()
    mock_consumer.return_value = mock_consumer_instance

    mock_message = Mock()
    mock_message.value = b"test-event"
    mock_consumer_instance.poll.return_value = {
        "test-topic-name": [mock_message],
    }

    subscriber = event.Subscriber(
        bootstrap_server_url="bootstrap_server_url",
        topic_names=["test-topic-name"],
    )

    events = subscriber.receive_events()

    assert events == ["test-event"]
    mock_consumer_instance.poll.assert_called_once_with(timeout_ms=1000)
