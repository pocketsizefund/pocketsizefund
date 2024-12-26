import polars as pl
from preprocessor import Preprocessor, DataLoader
from attention import (
    InterpretableMultiHeadAttention,
    ScaledDotProductAttention,
    bmm,
    tensor_astype,
    masked_fill,
)
from tinygrad.dtype import dtypes
from tinygrad import Tensor
import numpy as np
from embedding import get_embedding_size, MultiEmbedding, TimeDistributedEmbeddingBag, Embedding
from typing import Dict, List, Optional, Tuple
import torch


# size = get_embedding_size(1, 100)

# print(size)

# time_distributed_embedding_bag = TimeDistributedEmbeddingBag(5, 10)

# t = Tensor(np.random.rand(2, 3, 5).astype(np.float32))

# output = time_distributed_embedding_bag.forward(t)

# print(output)

# feature_count = 5
# embedding_dimension = 3

# embedding = TimeDistributedEmbeddingBag(
#     feature_count=feature_count,
#     embedding_dimension=embedding_dimension,
# )

# indices = Tensor.uniform((2, 4, feature_count))

# # indices = Tensor(np.array([[0, 1, 3], [2, 3, 4]]))  # Batch of 2, sequence length 3

# output = embedding.forward(indices)

# print("Indices:", indices.numpy())
# print("Embeddings:", output.numpy())

# test_dict = dict(Dict[str, Tensor])

# Assume batch_size = 2, sequence_length = 3
batch_size = 2
sequence_length = 3

# Example OHLCV data (5 numerical features)
ohlcv = torch.randn(batch_size, sequence_length, 5)  # Shape: (2, 3, 5)

# Example categorical data
ticker_indices = torch.randint(0, 30, (batch_size, sequence_length))  # Shape: (2, 3), 30 tickers
timestamp_encoded = torch.randint(
    0, 24, (batch_size, sequence_length)
)  # Shape: (2, 3), e.g., hour of day

# Combine the categorical features for input to MultiEmbedding
x = torch.stack([ticker_indices, timestamp_encoded], dim=-1)  # Shape: (2, 3, 2)

x = Tensor(x.numpy())

embedding_sizes = {"ticker": (30, 10), "timestamp": (24, 8)}
x_categoricals = ["ticker", "timestamp"]

categorical_groups = {}

# embedding_sizes = {
#     "feature1": (10, 4),  # 10 unique values, embedding size 4
#     "feature2": (20, 6),  # 20 unique values, embedding size 6
#     "group1": (15, 5),  # Group of 15 unique values, embedding size 5
# }

# x_categoricals = ["feature1", "feature2", "group1_featureA", "group1_featureB"]

# categorical_groups = {
#     "group1": ["group1_featureA", "group1_featureB"],
# }

# x = Tensor(
#     [
#         [  # Batch 1
#             [1, 3, 2, 5],  # Timestep 1
#             [0, 4, 1, 6],  # Timestep 2
#             [3, 2, 3, 4],  # Timestep 3
#         ],
#         [  # Batch 2
#             [2, 1, 4, 0],  # Timestep 1
#             [4, 5, 3, 2],  # Timestep 2
#             [1, 0, 2, 1],  # Timestep 3
#         ],
#     ]
# )

multi_embedding = MultiEmbedding(
    embedding_sizes=embedding_sizes,
    x_categoricals=x_categoricals,
    categorical_groups=categorical_groups,
)

output = multi_embedding.forward(x)

print(output)

# ==============================================================================

# outline:
# [x] multi embedding
# - [x] init
# - [x] forward
# [x] get embedding size
# [x] time distributed embedding bag
# - [x] init
# - [x] forward
# [x] embedding
# - [x] init
# - [x] forward
