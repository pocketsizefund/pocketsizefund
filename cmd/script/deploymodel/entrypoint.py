import os
import pickle
import json

import flask
import pandas

from pkg.model import model
from pkg.features import features


app = flask.Flask(__name__)


scalers_file = open(os.getenv("MODEL_DIR")+"/scalers.pkl", "rb")
scalers = pickle.load(scalers_file)

model_model = model.Model(
    artifact_output_path=os.getenv("MODEL_DIR"),
    weights_and_biases_api_key="",
)

model_model.load_model()

model_model.load_scalers()

features_client = features.Client()


@app.route("/invocations", methods=["POST"])
def invocations() -> flask.Response:
    json_data = flask.request.get_json()

    input_data = pandas.DataFrame(
        data=json_data,
    )

    features_data = features_client.generate_features(
        data=input_data,
    )

    preprocessed_features = model_model.preprocess_predicting_features(
        data=features_data,
        scalers=scalers,
    )

    predictions = model_model.generate_predictions(
        features=preprocessed_features,
    )

    return flask.Response(
        response=json.dumps(predictions),
        status=200,
    )


@app.route("/ping", methods=["GET"])
def ping() -> flask.Response:
    return flask.Response(
        response="",
        status=200,
        mimetype="application/json",
    )


if __name__ == "__main__":
    app.run(
        host="0.0.0.0",
        port=8080,
    )
