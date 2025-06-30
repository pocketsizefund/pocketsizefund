import os
from datetime import datetime, timedelta
from zoneinfo import ZoneInfo

import requests
from fastapi import FastAPI, Response, status
from loguru import logger
from prometheus_fastapi_instrumentator import Instrumentator

from .models import TriggerEventRequest

application = FastAPI()
Instrumentator().instrument(application).expose(application)


@application.get("/health")
def health_check() -> Response:
    return Response(status_code=status.HTTP_200_OK)


@application.post("/trigger")
def trigger_event(request: TriggerEventRequest) -> Response:
    event_type = request.event
    try:
        url = ""
        data = {}
        method = ""

        if event_type == "fetch_data":
            eastern_timezone = ZoneInfo("America/New_York")

            current_time = datetime.now(eastern_timezone)

            rounding_minute_threshold = 30  # threshold for rounding to the next hour

            rounded_time = None
            if current_time.minute >= rounding_minute_threshold:
                rounded_time = current_time.replace(
                    minute=0, second=0, microsecond=0
                ) + timedelta(hours=1)
            else:
                rounded_time = current_time.replace(minute=0, second=0, microsecond=0)

            datamanager_base_url = os.getenv("DATAMANAGER_BASE_URL")

            url = f"{datamanager_base_url}/equity-bars"

            data = {"date": rounded_time.strftime("%Y-%m-%d")}

            method = "POST"

        elif event_type == "create_positions":
            predictionengine_base_url = os.getenv("PREDICTIONENGINE_BASE_URL")

            url = f"{predictionengine_base_url}/create-positions"

            method = "POST"

        elif event_type == "close_positions":
            positionmanager_base_url = os.getenv("POSITIONMANAGER_BASE_URL")

            url = f"{positionmanager_base_url}/positions"

            method = "DELETE"

        else:
            logger.warning(f"Unknown event type: {event_type}")
            return Response(status_code=status.HTTP_400_BAD_REQUEST)

        if method == "POST":
            response = requests.post(
                url,
                json=data,
                timeout=10,
            )
            response.raise_for_status()
            logger.info(f"Event {event_type} triggered successfully.")

        elif method == "DELETE":
            response = requests.delete(
                url,
                timeout=10,
            )
            response.raise_for_status()
            logger.info(f"Event {event_type} triggered successfully.")

        else:
            logger.error(f"Unsupported HTTP method: {method}")
            return Response(status_code=status.HTTP_400_BAD_REQUEST)

        return Response(status_code=status.HTTP_200_OK)

    except requests.RequestException as e:
        logger.error(f"Error triggering event {event_type}: {e}")
        return Response(
            status_code=status.HTTP_500_INTERNAL_SERVER_ERROR,
            content=f"Error triggering event: {e}",
        )
