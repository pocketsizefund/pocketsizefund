from library.events import build_response_event


def test_build_response_event() -> None:
    event = build_response_event(
        "service_name",
        ["metadata"],
        {"key": "value"},
    )

    assert event["id"]
    assert event["specversion"] == "1.0"
    assert event["datacontenttype"] == "application/cloudevents+json"
    assert event["type"] == "pocketsizefund.service_name.metadata"
    assert event["source"] == "platform:service_name"
    assert event["extension"]["timestamp"]
