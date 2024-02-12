import json
import os
import pickle

import flask
import pandas
import numpy
from keras import models

from pkg.features import features


app = flask.Flask(__name__)


scalers_file = open(os.getenv('MODEL_DIR')+'/scalers.pkl', 'rb')
scalers = pickle.load(scalers_file)

features_client = features.Client()

model = models.load_model(
    filepath=os.getenv('MODEL_DIR')+'/lstm.keras',
)


@app.route('/invocations', methods=['POST'])
def invocations() -> flask.Response:
    json_data = flask.request.get_json()

    input_data = pandas.DataFrame(
        data=json_data,
    )

    features_data = features_client.generate_features(
        data=input_data,
    )

    preprocessed_features = features_client.preprocess_predicting_features(
        data=features_data,
        scalers=scalers,
    )

    predictions: dict[str, any] = {}
    for ticker, ticker_features in preprocessed_features.items():
        prediction = model.predict(
            x=ticker_features,
            verbose=0,
        )

        prediction = numpy.squeeze(prediction, axis=0)
        unscaled_predictions = scalers[ticker].inverse_transform(
            X=prediction,
        )

        predictions[ticker] = unscaled_predictions.tolist()

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
