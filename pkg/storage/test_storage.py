import gzip
import io

import pandas as pd

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

            dataframe = pd.read_csv(io.BytesIO(decompressed_data))

            self.data[key] = dataframe

        else:
            self.data[key] = body

    def get_object(
        self,
        **kwargs: any,
    ) -> any:
        key = kwargs["Key"]

        if isinstance(self.data[key], pd.DataFrame):
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

        return {
            "Body": io.BytesIO(self.data[key].encode()),
        }


def test_list_file_names_success() -> None:
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

    assert len(file_names) == 3
    assert file_names[0] == "2021"
    assert file_names[1] == "2022"
    assert file_names[2] == "first"


def test_store_dataframes_success() -> None:
    client = storage.Client(
        s3_data_bucket_name="s3_data_bucket_name",
        s3_artifacts_bucket_name="s3_artifacts_bucket_name",
    )

    client.s3_client = MockS3Client(
        data={},
    )

    first_dataframe = pd.DataFrame(data={"a": [1, 2]})
    second_dataframe = pd.DataFrame(data={"b": [3, 4]})

    dataframes = {
        "first": first_dataframe,
        "second": second_dataframe,
    }

    client.store_dataframes(
        prefix="prefix",
        dataframes_by_file_name=dataframes,
    )

    assert len(client.s3_client.data) == 2
    assert first_dataframe.equals(client.s3_client.data["prefix/first"])
    assert second_dataframe.equals(client.s3_client.data["prefix/second"])


def test_load_dataframes_success() -> None:
    client = storage.Client(
        s3_data_bucket_name="s3_data_bucket_name",
        s3_artifacts_bucket_name="s3_artifacts_bucket_name",
    )

    client.s3_client = MockS3Client(
        data={
            "prefix/first": pd.DataFrame(data={"a": [1, 2]}),
            "prefix/second": pd.DataFrame(data={"b": [3, 4]}),
        },
    )

    dataframes_by_file_name = client.load_dataframes(
        prefix="prefix",
        file_names=["first", "second"],
    )

    assert len(dataframes_by_file_name) == 2
    assert dataframes_by_file_name["first"].equals(client.s3_client.data["prefix/first"])
    assert dataframes_by_file_name["second"].equals(client.s3_client.data["prefix/second"])


def test_store_texts_success() -> None:
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

    assert len(client.s3_client.data) == 1
    assert text == client.s3_client.data["prefix/text.txt"]


def test_load_texts_success() -> None:
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

    assert texts_by_file_name["text.txt"] == "text"
