from fastapi import FastAPI, Query
from datetime import date
from typing import Dict, List, Union

app = FastAPI()

@app.get("/health")
async def health_check():
    return {"status": "healthy"}

@app.get("/v2/aggs/grouped/locale/us/market/stocks/{date}")
async def get_grouped_aggs(date: str, adjusted: str = "true", apiKey: str = None):
    """
    Mock endpoint for Polygon's grouped aggregates.
    """
    # Simple mock data for testing
    mock_data = [
        {"T": "AAPL", "o": 150.0, "h": 155.0, "l": 149.0, "c": 152.5, "v": 10000000, "n": 1000},
        {"T": "MSFT", "o": 250.0, "h": 255.0, "l": 248.0, "c": 253.0, "v": 5000000, "n": 500},
        {"T": "GOOG", "o": 2500.0, "h": 2550.0, "l": 2495.0, "c": 2540.0, "v": 1000000, "n": 200},
    ]
    
    return {
        "status": "OK",
        "request_id": "mock_request_123",
        "results": mock_data,
        "resultsCount": len(mock_data),
        "adjusted": adjusted == "true"
    }