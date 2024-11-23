"""CloudEvent helper resources."""

import datetime
import uuid
from typing import Any

from library.config import TIMEZONE


def build_response_event(
    service_name: str,
    metadata: list[str],
    json_payload: dict[str, Any],
) -> dict[str, Any]:
    """Build a CloudEvent response object."""
    tag = "pocketsizefund." + service_name + "." + ".".join(metadata)

    source = "platform:" + service_name

    return {
        "id": str(uuid.uuid4()),
        "specversion": "1.0",
        "datacontenttype": "application/cloudevents+json",
        "type": tag,
        "source": source,
        "extension": {
            "timestamp": datetime.datetime.now(TIMEZONE).isoformat(),
        },
        "data": json_payload,
    }
