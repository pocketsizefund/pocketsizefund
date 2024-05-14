import unittest

import pandas
import numpy
import tensorflow

from pkg.features import features


test_data = pandas.read_csv("pkg/features/test_data.csv")


class TestGenerateFeatures(unittest.TestCase):
    def test_generate_features_success(self) -> None:
        client = features.Client()

        result = client.generate_features(test_data)

        self.assertIsInstance(result, pandas.DataFrame)

        self.assertSetEqual(
            set(result.columns),
            {
                "timestamp",
                "ticker",
                "open_price",
                "high_price",
                "low_price",
                "close_price",
                "volume",
                "source",
            },
        )


class TestPreprocessTrainingData(unittest.TestCase):
    def test_preprocess_training_features_without_mocks_success(self) -> None:
        client = features.Client()

        result = client.preprocess_training_features(test_data)

        self.assertIsInstance(result, dict)
        self.assertSetEqual(set(result.keys()), {"data", "scalers"})

        data_keys = ["training", "validating", "testing"]
        self.assertSetEqual(set(result["data"].keys()), set(data_keys))

        self.assertIsInstance(result["scalers"], dict)
        self.assertEqual(len(result["scalers"]), 2)


class TestCreateDataset(unittest.TestCase):
    def test_create_dataset_success(self) -> None:
        client = features.Client()

        data = numpy.array(
            object=[
                [1, 2, 3, 4, 5],
                [4, 5, 6, 7, 8],
                [7, 8, 9, 10, 11],
                [10, 11, 12, 13, 14],
                [13, 14, 15, 16, 17],
                [16, 17, 18, 19, 20],
            ],
            dtype=numpy.float32,
        )

        dataset = client._create_dataset(data)

        self.assertIsInstance(
            dataset,
            tensorflow.data.Dataset,
        )

        for inputs, labels in dataset:
            self.assertEqual(inputs.shape, (32, 5, 3))
            self.assertEqual(labels.shape, (32, 2, 3))

            expected_inputs = data[:, :5, :]
            expected_labels = data[:, 3:5, :]

            numpy.testing.assert_array_equal(inputs.numpy(), expected_inputs)
            numpy.testing.assert_array_equal(labels.numpy(), expected_labels)


class TestSplitWindow(unittest.TestCase):
    def test_split_window_data_success(self) -> None:
        client = features.Client()

        client.window_input_length = 3
        client.window_output_length = 2

        data = numpy.array(
            object=[
                [
                    [1, 2, 3, 4, 5],
                    [3, 4, 5, 6, 7],
                    [5, 6, 7, 8, 9],
                    [7, 8, 9, 10, 11],
                    [9, 10, 11, 12, 13],
                ],
                [
                    [11, 12, 13, 14, 15],
                    [13, 14, 15, 16, 17],
                    [15, 16, 17, 18, 19],
                    [17, 18, 19, 20, 21],
                    [19, 20, 21, 22, 23],
                ],
                [
                    [21, 22, 23, 24, 25],
                    [23, 24, 25, 26, 27],
                    [25, 26, 27, 28, 29],
                    [27, 28, 29, 30, 31],
                    [29, 30, 31, 32, 33],
                ],
            ],
            dtype=numpy.float32,
        )

        result = client._split_window(data=tensorflow.constant(data))

        inputs, labels = result

        day_count = 3
        feature_count = 5
        label_count = 1

        expected_input_shape = (
            day_count,
            client.window_input_length,
            feature_count,
        )
        expected_labels_shape = (
            day_count,
            client.window_output_length,
            label_count,
        )

        self.assertEqual(inputs.shape.as_list(), list(expected_input_shape))
        self.assertEqual(labels.shape.as_list(), list(expected_labels_shape))

        expected_inputs_values = numpy.array(
            object=[
                [
                    [1, 2, 3, 4, 5],
                    [3, 4, 5, 6, 7],
                    [5, 6, 7, 8, 9],
                ],
                [
                    [11, 12, 13, 14, 15],
                    [13, 14, 15, 16, 17],
                    [15, 16, 17, 18, 19],
                ],
                [
                    [21, 22, 23, 24, 25],
                    [23, 24, 25, 26, 27],
                    [25, 26, 27, 28, 29],
                ],
            ],
            dtype=numpy.float32,
        )

        expected_labels_values = numpy.array(
            object=[
                [
                    [10],
                    [12],
                ],
                [
                    [20],
                    [22],
                ],
                [
                    [30],
                    [32],
                ],
            ],
            dtype=numpy.float32,
        )

        numpy.testing.assert_array_equal(
            inputs.numpy(),
            expected_inputs_values,
        )
        numpy.testing.assert_array_equal(
            labels.numpy(),
            expected_labels_values,
        )


class TestCleanAndGroupData(unittest.TestCase):
    def test_clean_and_group_data_success(self) -> None:
        client = features.Client()

        input = pandas.DataFrame(
            {
                "ticker": [
                    "AAPL",
                    "AAPL",
                    "AAPL",
                    "AAPL",
                    "AAPL",
                    "AAPL",
                ],
                "timestamp": [
                    "2024-01-01 16:00:00",
                    "2024-01-02 16:00:00",
                    "2024-01-03 16:00:00",
                    "2024-01-04 16:00:00",
                    "2024-01-05 16:00:00",
                    "2024-01-05 16:00:00",
                ],
                "open_price": [180.0, 182.0, 181.5, 183.0, 182.5, 182.5],
                "high_price": [182.5, 183.5, 183.0, 184.0, 183.5, 183.5],
                "low_price": [179.5, 181.0, 181.0, 182.0, 182.0, 182.0],
                "close_price": [182.0, 182.5, 182.5, 183.5, 183.0, 183.0],
                "volume": [1000, 1500, 1200, 2000, 1800, 1800],
                "source": [
                    "ALPACA",
                    "ALPACA",
                    "ALPACA",
                    "ALPACA",
                    "ALPACA",
                    "ALPACA",
                ],
            }
        )

        output = client._clean_and_group_data(input)

        self.assertTrue("AAPL" in output)
        self.assertFalse("ticker" in output["AAPL"].columns)
        self.assertFalse("source" in output["AAPL"].columns)
        self.assertEqual(output["AAPL"].index.isin(["2024-01-05 16:00:00"]).sum(), 1)
