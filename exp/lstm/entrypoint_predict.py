import json
import os
import pickle

import flask
import pandas
import numpy
from keras import models

from pkg.storage import storage
from pkg.data import data


app = flask.Flask(__name__)


storage_client = storage.Client(
    s3_data_bucket_name=os.getenv('S3_DATA_BUCKET_NAME'),
)

scalers_file = open(os.getenv('MODEL_DIR')+'/scalers.pkl', 'rb')
scalers = pickle.load(scalers_file)

data_client = data.Client(
    alpha_vantage_api_key=os.getenv('ALPHA_VANTAGE_API_KEY'),
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

    input_data.set_index(
        keys='timestamp',
        inplace=True,
    )

    input_data.drop(
        columns=['source'],
        inplace=True,
    )

    def convert_ticker_to_integer(ticker: str) -> int:
        return int.from_bytes(ticker.encode(), 'little')

    def convert_integer_to_ticker(integer: int) -> str:
        return integer.to_bytes((integer.bit_length() + 7) // 8, 'little').decode()

    input_data['ticker'] = input_data['ticker'].apply(
        convert_ticker_to_integer
    )

    input_data_grouped_by_ticker = input_data.groupby(
        by='ticker',
        dropna=True,
    )

    predictions = {}
    for ticker, ticker_data in input_data_grouped_by_ticker:
        ticker_data.sort_index(
            ascending=True,
            inplace=True,
        )

        if ticker not in scalers:
            continue  # TEMP

        scaled_ticker_data = scalers[ticker]['input'].fit_transform(
            X=ticker_data.values,
        )

        input_data = scaled_ticker_data.reshape(
            scaled_ticker_data.shape[0],
            1,
            scaled_ticker_data.shape[1],
        )

        prediction = model.predict(
            x=input_data,
            verbose=0,
        )

        scaler = scalers[ticker]['output']

        unscaled_predictions = scaler.inverse_transform(
            X=numpy.squeeze(
                a=prediction,
                axis=1,
            ),
        )

        predictions[
            convert_integer_to_ticker(ticker)
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
