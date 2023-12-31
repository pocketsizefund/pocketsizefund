import json
import os
import pickle

import flask
import pandas
import numpy
from keras import models

from pkg.data import data
import entrypoint_helpers


app = flask.Flask(__name__)


scalers_file = open(os.getenv('MODEL_DIR')+'/scalers.pkl', 'rb')
scalers = pickle.load(scalers_file)

data_client = data.Client(
    alpaca_api_key=os.getenv('ALPACA_API_KEY'),
    alpaca_api_secret=os.getenv('ALPACA_API_SECRET'),
)

model = models.load_model(
    filepath=os.getenv('MODEL_DIR')+'/lstm.keras',
)


@app.route('/invocations', methods=['POST'])
def invocations() -> flask.Response:
    json_data = flask.request.get_json()

    input_data = pandas.DataFrame(
        data=json_data,
    )

    preprocessed_data = entrypoint_helpers.preprocess_predicting_data(
        data=input_data,
        scalers=scalers,
    )

    predictions: dict[str, any] = {}

    for ticker, ticker_data in preprocessed_data.items():
        prediction = model.predict(
            x=ticker_data,
            verbose=0,
        )

        unscaled_predictions = scalers[ticker].inverse_transform(
            X=numpy.squeeze(
                a=prediction,
                axis=1,
            ),
        )

        predictions[
            entrypoint_helpers.convert_integer_to_ticker(ticker)
        ] = unscaled_predictions.tolist()

    return flask.Response(
        response=json.dumps(predictions),
        status=200,
    )


@app.route('/ping', methods=['GET'])
def ping() -> flask.Response:
    return flask.Response(
        response='',
        status=200,
        mimetype='application/json',
    )


if __name__ == '__main__':
    app.run(
        host='0.0.0.0',
        port=8080,
    )
