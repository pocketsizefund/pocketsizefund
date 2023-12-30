import pandas
from sklearn import preprocessing
import numpy
import tensorflow
import keras


WINDOW_INPUT_LENGTH = 30
WINDOW_OUTPUT_LENGTH = 5


def preprocess_training_data(
    data: pandas.DataFrame,
    splits: tuple[float, float, float] = (0.7, 0.2, 0.1),
) -> dict[str, any]:
    close_price_index = data.columns.get_loc('close_price')

    data_grouped_by_ticker = _clean_and_group_data(data)

    scalers: dict[int, preprocessing.MinMaxScaler] = {}

    scaled_training_data: list[numpy.ndarray] = []
    scaled_validating_data: list[numpy.ndarray] = []
    scaled_testing_data: list[numpy.ndarray] = []

    for ticker, ticker_data in data_grouped_by_ticker:
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

        break  # TEMP -----------------------------

    training_datasets = list(map(lambda x: _create_dataset(
        data=x,
        close_price_index=close_price_index,
    ), scaled_training_data))

    validating_datasets = list(map(lambda x: _create_dataset(
        data=x,
        close_price_index=close_price_index,
    ), scaled_validating_data))

    testing_datasets = list(map(lambda x: _create_dataset(
        data=x,
        close_price_index=close_price_index,
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


def _create_dataset(
    data: numpy.ndarray,
    close_price_index: int,
) -> tensorflow.data.Dataset:
    dataset = keras.utils.timeseries_dataset_from_array(
        data=data,
        targets=None,
        sequence_length=WINDOW_OUTPUT_LENGTH+WINDOW_INPUT_LENGTH,
        sequence_stride=1,
        shuffle=True,
        batch_size=32,
    )

    windowed_dataset = dataset.map(
        lambda x: _split_window(x, close_price_index)
    )

    return windowed_dataset


def _split_window(
    data: tensorflow.Tensor,
    close_price_index: int,
) -> tensorflow.data.Dataset:
    input_slice = slice(0, WINDOW_INPUT_LENGTH)
    labels_slice = slice(WINDOW_INPUT_LENGTH, None)

    inputs = data[:, input_slice, :]
    labels = data[:, labels_slice, :]

    labels = tensorflow.stack(
        values=[labels[:, :, close_price_index]],
        axis=-1,
    )

    inputs.set_shape([None, WINDOW_INPUT_LENGTH, None])
    labels.set_shape([None, WINDOW_OUTPUT_LENGTH, None])

    return (inputs, labels)


def preprocess_predicting_data(
    data: pandas.DataFrame,
    scalers: dict[int, preprocessing.MinMaxScaler],
) -> dict[str, tensorflow.data.Dataset]:
    data_grouped_by_ticker = _clean_and_group_data(data)

    predicing_datasets: dict[str, tensorflow.data.Dataset] = {}

    for ticker, ticker_data in data_grouped_by_ticker:
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

        predicing_datasets[ticker] = dataset

    return predicing_datasets


def _clean_and_group_data(data: pandas.DataFrame) -> pandas.DataFrame:
    data.dropna(
        inplace=True,
    )

    data.drop_duplicates(
        subset=[
            'timestamp',
            'ticker',
        ],
        inplace=True,
    )

    data.drop(
        columns=['source'],
        inplace=True,
    )

    data.set_index(
        keys='timestamp',
        inplace=True,
    )

    data['ticker'] = data['ticker'].apply(_convert_ticker_to_integer)

    data_grouped_by_ticker = data.groupby(
        by='ticker',
        dropna=True,
    )

    return data_grouped_by_ticker


def convert_ticker_to_integer(ticker: str) -> int:
    return _convert_ticker_to_integer(ticker)


def _convert_ticker_to_integer(ticker: str) -> int:
    return int.from_bytes(ticker.encode(), 'little')


def convert_integer_to_ticker(integer: int) -> str:
    return integer.to_bytes((integer.bit_length() + 7) // 8, 'little').decode()
