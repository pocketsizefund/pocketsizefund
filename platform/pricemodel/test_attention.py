from attention import InterpretableMultiHeadAttention, ScaledDotProductAttention
from tinygrad import Tensor


scaled_dot_product_attention = ScaledDotProductAttention()

batch_size = 2  # Number of sequences in the batch
num_queries = 4  # Number of queries
num_keys = 6  # Number of keys (also determines values)
d_k = 8  # Dimension of keys and queries
d_v = 10  # Dimension of values

q = Tensor.randn(batch_size, num_queries, d_k)
k = Tensor.randn(batch_size, num_keys, d_k)
v = Tensor.randn(batch_size, num_keys, d_v)

output, attention = scaled_dot_product_attention.forward(q, k, v)

heads_count = 4
models_count = 16
dropout_rate = 0.1

interpretable_multi_head_attention = InterpretableMultiHeadAttention(
    heads_count=heads_count,
    models_count=models_count,
    dropout_rate=dropout_rate,
)

# Define input dimensions
batch_size = 2
sequence_length = 5

# Create random input data
q = Tensor.randn(batch_size, sequence_length, models_count)  # Query tensor
k = Tensor.randn(batch_size, sequence_length, models_count)  # Key tensor
v = Tensor.randn(batch_size, sequence_length, models_count)  # Value tensor

mask = None

# mask = Tensor.randint(0, 2, sequence_length).bool()  # Mask tensor

outputs, attention = interpretable_multi_head_attention.forward(q, k, v, mask)

print(outputs)
print(attention)

# ==============================================================================

# [x] interpretable multi head attention
# - [x] init
# - [x] forward
# [x] scaled dot product attention
# - [x] forward
# - [x] masked fill
# - [x] bmm
# - [x] tensor astype
