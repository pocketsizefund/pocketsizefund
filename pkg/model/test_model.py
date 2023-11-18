import unittest

import pandas

from pkg.model import model


class MockPredictor:
    def __init__(self) -> None:
        pass

    def predict(
        self,
        data: any,
    ) -> any:
        return {
            '65': [
                [
                    10.0,
                ],
            ],
        }


class TestGeneratePredictions(unittest.TestCase):
    def test_generate_predictions_success(self):
        client = model.Client(
            model_endpoint_name='model_endpoint_name',
        )

        client.predictor = MockPredictor()

        predictions = client.generate_predictions(data=pandas.DataFrame(
            data={
                'timestamp': pandas.Timestamp('2021-01-01'),
            },
            index=[0],
        ))

        self.assertEqual(10.0, predictions['65'][0][0])
