from datetime import datetime
from zoneinfo import ZoneInfo

from cloudevents.pydantic.v2 import CloudEvent


def create_cloud_event_success(
    application_name: str,
    event_metadata: list[str],
    data: dict,
) -> CloudEvent:
    return CloudEvent(
        attributes={
            "source": application_name,
            "type": f"application.{application_name}.{'.'.join(event_metadata)}",
        },
        data={
            "date": datetime.now(tz=ZoneInfo("America/New_York")).isoformat(),
            **data,
        },
    )


def create_cloud_event_error(
    application_name: str,
    event_metadata: list[str],
    error_message: str,
) -> CloudEvent:
    return CloudEvent(
        attributes={
            "source": application_name,
            "type": f"application.{application_name}.{'.'.join(event_metadata)}",
        },
        data={
            "date": datetime.now(tz=ZoneInfo("America/New_York")).isoformat(),
            "error": error_message,
        },
    )
