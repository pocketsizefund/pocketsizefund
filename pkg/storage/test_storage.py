import unittest

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
