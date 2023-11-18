from sagemaker import tensorflow
import pandas


class Client:
    def __init__(
        self,
        model_endpoint_name: str,
    ) -> None:
        self.predictor = tensorflow.TensorFlowPredictor(
            endpoint_name=model_endpoint_name,
        )

    def generate_predictions(
        self,
        data: pandas.DataFrame,
    ) -> any:
        data['timestamp'] = data['timestamp'].astype(str)

        predictions = self.predictor.predict(
            data=data.to_dict(orient='records'),
        )

        return predictions
