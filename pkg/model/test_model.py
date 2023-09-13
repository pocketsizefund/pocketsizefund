import unittest

import pandas
from sklearn import preprocessing

from pkg.model import model


class TestPreprocessData(unittest.TestCase):
    def test_preprocess_data_succes(self):
        input_data = pandas.read_csv('pkg/model/test_data.csv')

        scalers: dict[str: preprocessing.MinMaxScaler] = {}

        tickers = input_data['ticker'].unique()
        for ticker in tickers:
            scalers[ticker] = preprocessing.MinMaxScaler(feature_range=(0, 1))

        client = model.Client(
            sort_column='timestamp',
            drop_columns=['source'],
            scale_columns=[
                'open_price',
                'high_price',
                'low_price',
                'close_price',
                'volume',
            ],
            group_column='ticker',
            target_column='close_price',
            scalers=scalers,
        )

        output_data = client.preprocess_data(
            data=input_data,
            training_percentage=0.8,
        )

        self.assertEqual(len(output_data), 10)
        self.assertEqual(len(output_data['BIIB']), 4)
        self.assertEqual(len(output_data['BIIB']['input_training_data']), 103)
        self.assertEqual(len(output_data['BIIB']['output_training_data']), 103)
        self.assertEqual(len(output_data['BIIB']['input_testing_data']), 4)
        self.assertEqual(len(output_data['BIIB']['output_testing_data']), 4)
