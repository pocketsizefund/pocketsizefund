from fastapi import FastAPI

application = FastAPI()


@application.post("/predictions")
async def create_predictions(data: dict) -> dict:
    _ = data
    # Dummy implementation for prediction
    return {"predictions": [0.1, 0.9]}

    # NOTE: this will use the sagemaker sdk predictor class to call the endpoint
