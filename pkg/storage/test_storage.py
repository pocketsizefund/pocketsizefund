import unittest
import gzip
import io

import pandas

from pkg.storage import storage


class MockS3Client:
    def __init__(
        self,
        data: dict[str, any],
    ) -> None:
        self.data = data

    def list_objects_v2(
        self,
        **kwargs: any,
    ) -> any:
        return self.data

    def put_object(
        self,
        **kwargs: any,
    ) -> None:
        key = kwargs['Key']
        body = kwargs['Body']

        decompressed_data = gzip.decompress(body)

        dataframe = pandas.read_csv(io.BytesIO(decompressed_data))

        self.data[key] = dataframe

    def get_object(
        self,
        **kwargs: any,
    ) -> any:
        key = kwargs['Key']

        dataframe = self.data[key]

        gzip_buffer = io.BytesIO()
        dataframe.to_csv(
            gzip_buffer,
            index=False,
            header=True,
            compression='gzip',
        )
        gzip_buffer.seek(0)

        return {
            'Body': gzip_buffer,
        }


class TestListFileNames(unittest.TestCase):
    def test_list_file_names_success(self):
        client = storage.Client(
            s3_data_bucket_name='s3_data_bucket_name',
        )

        client.s3_client = MockS3Client(
            data={
                'KeyCount': 3,
                'IsTruncated': False,
                'Contents': [
                    {
                        'Key': 'prefix/2021',
                    },
                    {
                        'Key': 'prefix/2022',
                    },
                    {
                        'Key': 'prefix/2023',
                    },
                ],
            },
        )

        file_names = client.list_file_names(
            prefix='prefix',
        )

        self.assertEqual(3, len(file_names))
        self.assertEqual('2021', file_names[0])
        self.assertEqual('2022', file_names[1])
        self.assertEqual('2023', file_names[2])


class TestGetNextPrefixVersion(unittest.TestCase):
    def test_get_next_prefix_version_no_files(self):
        client = storage.Client(
            s3_data_bucket_name='s3_data_bucket_name',
        )

        next_prefix_version = client.get_next_prefix_version(
            prefixes=[],
        )

        self.assertEqual('v0', next_prefix_version)

    def test_get_next_prefix_version_files(self):
        client = storage.Client(
            s3_data_bucket_name='s3_data_bucket_name',
        )

        next_prefix_version = client.get_next_prefix_version(
            prefixes=[
                'prefix/v0',
                'prefix/v1',
                'prefix/v2',
            ],
        )

        self.assertEqual('v3', next_prefix_version)


class TestGetMaxPrefixVersion(unittest.TestCase):
    def test_get_max_prefix_version_no_files(self):
        client = storage.Client(
            s3_data_bucket_name='s3_data_bucket_name',
        )

        max_prefix_version = client.get_max_prefix_version(
            prefixes=[],
        )

        self.assertEqual('', max_prefix_version)

    def test_get_max_prefix_version_files(self):
        client = storage.Client(
            s3_data_bucket_name='s3_data_bucket_name',
        )

        max_prefix_version = client.get_max_prefix_version(
            prefixes=[
                'prefix/v0',
                'prefix/v1',
                'prefix/v2',
            ],
        )

        self.assertEqual('v2', max_prefix_version)


class TestStoreDataframes(unittest.TestCase):
    def test_store_dataframes_success(self):
        client = storage.Client(
            s3_data_bucket_name='s3_data_bucket_name',
        )

        client.s3_client = MockS3Client(
            data={},
        )

        first_dataframe = pandas.DataFrame(data={'a': [1, 2]})
        second_dataframe = pandas.DataFrame(data={'b': [3, 4]})

        dataframes = {
            '2023': first_dataframe,
            '2024': second_dataframe,
        }

        client.store_dataframes(
            prefix='prefix',
            dataframes=dataframes,
        )

        self.assertEqual(2, len(client.s3_client.data))
        self.assertTrue(
            first_dataframe.equals(client.s3_client.data['prefix/2023']),
        )
        self.assertTrue(
            second_dataframe.equals(client.s3_client.data['prefix/2024']),
        )


class TestLoadDataframes(unittest.TestCase):
    def test_load_dataframes_success(self):
        client = storage.Client(
            s3_data_bucket_name='s3_data_bucket_name',
        )

        client.s3_client = MockS3Client(
            data={
                'prefix/2023': pandas.DataFrame(data={'a': [1, 2]}),
                'prefix/2024': pandas.DataFrame(data={'b': [3, 4]}),
            },
        )

        dataframes = client.load_dataframes(
            prefix='prefix',
            file_names=['2023', '2024'],
        )

        self.assertEqual(2, len(dataframes))
        self.assertTrue(
            dataframes['2023'].equals(client.s3_client.data['prefix/2023']),
        )
        self.assertTrue(
            dataframes['2024'].equals(client.s3_client.data['prefix/2024']),
        )
