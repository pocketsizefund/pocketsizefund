from pydantic import BaseModel


class PredictionResponse(BaseModel):
    predictions: dict[str, dict[str, float]]
