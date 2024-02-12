import pandas
from sklearn import preprocessing
import numpy
import tensorflow
import keras

from pkg.data import data


WINDOW_INPUT_LENGTH = 30
WINDOW_OUTPUT_LENGTH = 5

FEATURES = {
    'open_price': data.COLUMN_OPEN_PRICE,
    'high_price': data.COLUMN_HIGH_PRICE,
    'low_price': data.COLUMN_LOW_PRICE,
    'close_price': data.COLUMN_CLOSE_PRICE,
    'volume': data.COLUMN_VOLUME,
}

COLUMNS = {
    'timestamp': data.COLUMN_TIMESTAMP,
    'ticker': data.COLUMN_TICKER,
    'source': data.COLUMN_SOURCE,
    **FEATURES,
}


def preprocess_training_features(
    data: pandas.DataFrame,
    splits: tuple[float, float, float] = (0.7, 0.2, 0.1),
) -> dict[str, any]:
    data_grouped_by_ticker = _clean_and_group_data(data)

    scalers: dict[int, preprocessing.MinMaxScaler] = {}

    scaled_training_data: list[numpy.ndarray] = []
    scaled_validating_data: list[numpy.ndarray] = []
    scaled_testing_data: list[numpy.ndarray] = []

    for ticker, ticker_data in data_grouped_by_ticker.items():
        count = len(ticker_data)

        if count < WINDOW_INPUT_LENGTH + WINDOW_OUTPUT_LENGTH:
            continue

        first_split = int(count * splits[0])
        second_split = int(count * (splits[0] + splits[1]))

        training_data = ticker_data[:first_split]
        validating_data = ticker_data[first_split:second_split]
        testing_data = ticker_data[second_split:]

        scaler = preprocessing.MinMaxScaler(feature_range=(0, 1))

        scaled_training_data.append(scaler.fit_transform(
            X=training_data.values,
        ))

        scaled_validating_data.append(scaler.transform(
            X=validating_data.values,
        ))

        scaled_testing_data.append(scaler.transform(
            X=testing_data.values,
        ))

        scalers[ticker] = scaler

    training_datasets = list(map(lambda x: _create_dataset(
        data=x,
    ), scaled_training_data))

    validating_datasets = list(map(lambda x: _create_dataset(
        data=x,
    ), scaled_validating_data))

    testing_datasets = list(map(lambda x: _create_dataset(
        data=x,
    ), scaled_testing_data))

    training_dataset = training_datasets[0]
    for dataset in training_datasets[1:]:
        training_dataset = training_dataset.concatenate(dataset)

    validating_dataset = validating_datasets[0]
    for dataset in validating_datasets[1:]:
        validating_dataset = validating_dataset.concatenate(dataset)

    testing_dataset = testing_datasets[0]
    for dataset in testing_datasets[1:]:
        testing_dataset = testing_dataset.concatenate(dataset)

    return {
        'data': {
            'training': training_dataset,
            'validating': validating_dataset,
            'testing': testing_dataset,
        },
        'scalers': scalers,
    }


def _create_dataset(data: numpy.ndarray) -> tensorflow.data.Dataset:
    dataset = keras.utils.timeseries_dataset_from_array(
        data=data,
        targets=None,
        sequence_length=WINDOW_OUTPUT_LENGTH+WINDOW_INPUT_LENGTH,
        sequence_stride=1,
        shuffle=True,
        batch_size=32,
    )

    windowed_dataset = dataset.map(
        lambda x: _split_window(x)
    )

    return windowed_dataset


@tensorflow.autograph.experimental.do_not_convert  # suppress warning
def _split_window(
    data: tensorflow.Tensor,
) -> tensorflow.data.Dataset:
    input_slice = slice(0, WINDOW_INPUT_LENGTH)
    labels_slice = slice(WINDOW_INPUT_LENGTH, None)

    inputs = data[:, input_slice, :]
    labels = data[:, labels_slice, :]

    labels = tensorflow.stack(
        values=[labels],
        axis=-1,
    )

    labels = tensorflow.squeeze(labels, axis=-1)

    inputs.set_shape([None, WINDOW_INPUT_LENGTH, None])
    labels.set_shape([None, WINDOW_OUTPUT_LENGTH, None])

    return (inputs, labels)


def preprocess_predicting_features(
    data: pandas.DataFrame,
    scalers: dict[int, preprocessing.MinMaxScaler],
) -> dict[str, tensorflow.data.Dataset]:
    data_grouped_by_ticker = _clean_and_group_data(data)

    predicting_datasets: dict[str, tensorflow.data.Dataset] = {}

    for ticker, ticker_data in data_grouped_by_ticker.items():
        count = len(ticker_data)

        if count < WINDOW_INPUT_LENGTH:
            continue

        if ticker not in scalers:
            continue

        scaled_ticker_data = scalers[ticker].transform(
            X=ticker_data.values,
        )

        dataset = keras.utils.timeseries_dataset_from_array(
            data=scaled_ticker_data,
            targets=None,
            sequence_length=WINDOW_INPUT_LENGTH,
            shuffle=True,
        )

        predicting_datasets[ticker] = dataset

    return predicting_datasets


def _clean_and_group_data(data: pandas.DataFrame) -> dict[str, pandas.DataFrame]:
    data.dropna(
        inplace=True,
    )

    data.drop_duplicates(
        subset=[
            COLUMNS['timestamp'],
            COLUMNS['ticker'],
        ],
        inplace=True,
    )

    data.drop(
        columns=[COLUMNS['source']],
        inplace=True,
    )

    data.set_index(
        keys=COLUMNS['timestamp'],
        inplace=True,
    )

    data_grouped_by_ticker = {
        str(ticker): ticker_group.drop(
            columns=[COLUMNS['ticker']],
        )
        for ticker, ticker_group
        in data.groupby(
            by=COLUMNS['ticker'],
            dropna=True,
        )
    }

    return data_grouped_by_ticker
