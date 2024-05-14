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
        key = kwargs["Key"]
        body = kwargs["Body"]

        if isinstance(body, bytes):
            decompressed_data = gzip.decompress(body)

            dataframe = pandas.read_csv(io.BytesIO(decompressed_data))

            self.data[key] = dataframe

        else:
            self.data[key] = body

    def get_object(
        self,
        **kwargs: any,
    ) -> any:
        key = kwargs["Key"]

        if isinstance(self.data[key], pandas.DataFrame):
            dataframe = self.data[key]

            gzip_buffer = io.BytesIO()
            dataframe.to_csv(
                gzip_buffer,
                index=False,
                header=True,
                compression="gzip",
            )
            gzip_buffer.seek(0)

            return {
                "Body": gzip_buffer,
            }

        else:
            return {
                "Body": io.BytesIO(self.data[key].encode()),
            }


class TestListFileNames(unittest.TestCase):
    def test_list_file_names_success(self):
        client = storage.Client(
            s3_data_bucket_name="s3_data_bucket_name",
            s3_artifacts_bucket_name="s3_artifacts_bucket_name",
        )

        client.s3_client = MockS3Client(
            data={
                "KeyCount": 3,
                "IsTruncated": False,
                "Contents": [
                    {
                        "Key": "prefix/2021",
                    },
                    {
                        "Key": "prefix/2022",
                    },
                    {
                        "Key": "prefix/first",
                    },
                ],
            },
        )

        file_names = client.list_file_names(
            prefix="prefix",
        )

        self.assertEqual(3, len(file_names))
        self.assertEqual("2021", file_names[0])
        self.assertEqual("2022", file_names[1])
        self.assertEqual("first", file_names[2])


class TestStoreDataframes(unittest.TestCase):
    def test_store_dataframes_success(self):
        client = storage.Client(
            s3_data_bucket_name="s3_data_bucket_name",
            s3_artifacts_bucket_name="s3_artifacts_bucket_name",
        )

        client.s3_client = MockS3Client(
            data={},
        )

        first_dataframe = pandas.DataFrame(data={"a": [1, 2]})
        second_dataframe = pandas.DataFrame(data={"b": [3, 4]})

        dataframes = {
            "first": first_dataframe,
            "second": second_dataframe,
        }

        client.store_dataframes(
            prefix="prefix",
            dataframes_by_file_name=dataframes,
        )

        self.assertEqual(2, len(client.s3_client.data))
        self.assertTrue(
            first_dataframe.equals(client.s3_client.data["prefix/first"]),
        )
        self.assertTrue(
            second_dataframe.equals(client.s3_client.data["prefix/second"]),
        )


class TestLoadDataframes(unittest.TestCase):
    def test_load_dataframes_success(self):
        client = storage.Client(
            s3_data_bucket_name="s3_data_bucket_name",
            s3_artifacts_bucket_name="s3_artifacts_bucket_name",
        )

        client.s3_client = MockS3Client(
            data={
                "prefix/first": pandas.DataFrame(data={"a": [1, 2]}),
                "prefix/second": pandas.DataFrame(data={"b": [3, 4]}),
            },
        )

        dataframes_by_file_name = client.load_dataframes(
            prefix="prefix",
            file_names=["first", "second"],
        )

        self.assertEqual(2, len(dataframes_by_file_name))
        self.assertTrue(
            dataframes_by_file_name["first"].equals(
                client.s3_client.data["prefix/first"]),
        )
        self.assertTrue(
            dataframes_by_file_name["second"].equals(
                client.s3_client.data["prefix/second"]),
        )


class TestStoreTexts(unittest.TestCase):
    def test_store_texts_success(self):
        client = storage.Client(
            s3_data_bucket_name="s3_data_bucket_name",
            s3_artifacts_bucket_name="s3_artifacts_bucket_name",
        )

        client.s3_client = MockS3Client(
            data={},
        )

        text = "text"

        client.store_texts(
            prefix="prefix",
            texts_by_file_name={"text.txt": text},
        )

        self.assertEqual(1, len(client.s3_client.data))
        self.assertEqual(
            text,
            client.s3_client.data["prefix/text.txt"],
        )


class TestLoadTexts(unittest.TestCase):
    def test_load_texts_success(self):
        client = storage.Client(
            s3_data_bucket_name="s3_data_bucket_name",
            s3_artifacts_bucket_name="s3_artifacts_bucket_name",
        )

        client.s3_client = MockS3Client(
            data={
                "prefix/text.txt": "text",
            },
        )

        texts_by_file_name = client.load_texts(
            prefix="prefix",
            file_names=["text.txt"],
        )

        self.assertEqual("text", texts_by_file_name["text.txt"])
