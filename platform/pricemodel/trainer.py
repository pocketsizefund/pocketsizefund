from tinygrad import Tensor
import polars as pl
from pricemodel.dataset import DataSet, continuous_variable_columns
from pricemodel.miniature_temporal_fusion_transformer import MiniatureTemporalFusionTransformer

# NOTE: runs on t4g.2xlarge

sequence_length = 30
epoch_count = 3

Tensor.training = True

training_data = pl.read_csv("platform/pricemodel/small_data.csv")

ticker_count = len(training_data["ticker"].unique())

training_dataset = DataSet(
    batch_size=ticker_count,
    sequence_length=sequence_length,
)

training_dataset.load_data(training_data)

preprocessors = training_dataset.get_preprocessors()

embedding_size = ticker_count // 2
input_size = len(continuous_variable_columns) + embedding_size
hidden_size = 32
output_size = 15  # 5 days * 3 quantiles
lstm_layer_count = 1
dropout_rate = 0.0
attention_head_count = 4

miniature_temporal_fusion_transformer_model = MiniatureTemporalFusionTransformer(
    ticker_count=ticker_count,
    embedding_size=embedding_size,
    feature_count=len(continuous_variable_columns),
    hidden_size=hidden_size,
    output_size=output_size,
    lstm_layer_count=lstm_layer_count,
    attention_head_count=attention_head_count,
)

training_losses = miniature_temporal_fusion_transformer_model.train(
    dataset=training_dataset,
    epoch_count=epoch_count,
    learning_rate=1e-3,
)

print("training_losses:", training_losses)
