from pydantic import BaseModel
from typing import Dict


class PredictionResponse(BaseModel):
    predictions: Dict[str, Dict[str, float]]
