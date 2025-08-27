from fastapi import FastAPI, Response, status
from structlog import get_logger

logger = get_logger()

app: FastAPI = FastAPI()


@app.get("/health")
def health_check() -> Response:
    return Response(status_code=status.HTTP_200_OK)


@app.get("/portfolio-check")
def check_portfolio() -> Response:
    logger.info("I was called by the portfoliomanager")

    return Response(status_code=status.HTTP_200_OK)
