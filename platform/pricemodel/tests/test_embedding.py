from pricemodel.embedding import (
    Embedding,
    TimeDistributedEmbeddingBag,
    get_embedding_size,
    MultiEmbedding,
)
from tinygrad import Tensor
from tinygrad.dtype import dtypes
import numpy as np


def test_embedding():
    feature_count = 50
    embedding_dimension = 16

    embedding = Embedding(feature_count, embedding_dimension)

    assert embedding.embedding_table.shape == (feature_count, embedding_dimension)

    embedding_numpy = embedding.embedding_table.numpy()  # Convert to NumPy array
    assert np.all(embedding_numpy >= -0.1) and np.all(embedding_numpy <= 0.1)

    assert isinstance(embedding.embedding_table, Tensor)

    batch_size = 8
    sequence_length = 10

    indices = Tensor(np.random.randint(0, feature_count, size=(batch_size, sequence_length)))

    output = embedding.forward(indices)

    assert output.shape == (batch_size, sequence_length, embedding_dimension)


def test_time_distributed_embedding_bag():
    feature_count = 10
    embedding_dimension = 5

    time_distributed_embedding_bag = TimeDistributedEmbeddingBag(feature_count, embedding_dimension)

    assert time_distributed_embedding_bag.embedding_table.shape == (
        feature_count,
        embedding_dimension,
    )

    embedding_table_numpy = time_distributed_embedding_bag.embedding_table.numpy()
    assert np.all(embedding_table_numpy >= -0.1) and np.all(embedding_table_numpy <= 0.1)

    batch_size = 2
    sequence_length = 3
    features_per_time = feature_count

    x = Tensor.uniform(
        (batch_size, sequence_length, features_per_time),
        low=0,
        high=1,
    )

    output = time_distributed_embedding_bag.forward(x)

    assert output.shape == (batch_size, embedding_dimension)

    output_numpy = output.numpy()
    assert np.all(output_numpy >= -0.1 * feature_count) and np.all(
        output_numpy <= 0.1 * feature_count
    )


def test_get_embedding_size():
    assert get_embedding_size(2) == 1, "Failed for class_count=2"
    assert get_embedding_size(1) == 1, "Failed for class_count=1"

    class_count = 10
    expected_size = round(1.6 * class_count**0.56)
    assert get_embedding_size(class_count) == expected_size, f"Failed for class_count={class_count}"

    class_count = 1000
    max_size = 50
    assert (
        get_embedding_size(class_count, max_size) == max_size
    ), f"Failed for class_count={class_count} with max_size={max_size}"

    class_count = 50
    max_size = round(1.6 * class_count**0.56)
    assert (
        get_embedding_size(class_count, max_size) == max_size
    ), f"Failed for class_count={class_count} with max_size={max_size}"

    class_count = 200
    assert (
        get_embedding_size(class_count) <= 100
    ), f"Failed for class_count={class_count} with default max_size=100"


def test_multi_embedding():
    embedding_sizes = {
        "feature1": (10, 5),
        "feature2": 20,
    }
    x_categoricals = ["feature1", "feature2", "feature3"]
    categorical_groups = {"group1": ["feature1", "feature3"]}

    multi_embedding = MultiEmbedding(embedding_sizes, x_categoricals, categorical_groups)

    assert multi_embedding.embedding_sizes == {
        "feature1": (10, 5),
        "feature2": (20, 9),
    }, "Incorrect embedding_sizes initialization"

    assert isinstance(multi_embedding.embeddings["feature1"], Embedding)
    assert isinstance(multi_embedding.embeddings["feature2"], Embedding)

    assert multi_embedding.output_size == {"feature1": 5, "feature2": 9}, "Incorrect output_size"

    batch_size = 2
    sequence_length = 3
    feature_count = len(x_categoricals)

    x = Tensor.uniform((batch_size, sequence_length, feature_count), low=0, high=1).cast(dtypes.int)

    output = multi_embedding.forward(x)

    assert "feature1" in output
    assert "feature2" in output
    assert output["feature1"].shape == (
        batch_size,
        sequence_length,
        5,
    ), "Incorrect shape for feature1"
    assert output["feature2"].shape == (
        batch_size,
        sequence_length,
        9,
    ), "Incorrect shape for feature2"
