from pricemodel.miniature_temporal_fusion_transformer import MiniatureTemporalFusionTransformer
from pricemodel.dataset import DataSet, continuous_variable_columns
import polars as pl
from tinygrad import Tensor

batch_size = 30
sequence_length = 30
# sequence_length = 5  # NOTE: should be 30 (sequence being the number of input days)


Tensor.training = True

training_data = pl.read_csv("platform/pricemodel/consolidated_data.csv")

training_dataset = DataSet(
    batch_size=batch_size,
    sequence_length=sequence_length,
)

training_dataset.load_data(training_data)

preprocessors = training_dataset.get_preprocessors()

ticker_count = len(training_data["ticker"].unique())
embedding_size = ticker_count // 2
input_size = len(continuous_variable_columns) + embedding_size
hidden_size = 64
# output_size = 5
# output_size = 1
output_size = 3  # quantiles
layer_count = 3
attention_head_count = 4
means_by_ticker = preprocessors["means_by_ticker"]
standard_deviations_by_ticker = preprocessors["standard_deviations_by_ticker"]
ticker_encoder = preprocessors["ticker_encoder"]
dropout_rate = 0.10

miniature_temporal_fusion_transformer_model = MiniatureTemporalFusionTransformer(
    input_size=input_size,
    hidden_size=hidden_size,
    output_size=output_size,
    layer_count=layer_count,
    ticker_count=ticker_count,
    embedding_size=embedding_size,
    attention_head_count=attention_head_count,
    means_by_ticker=means_by_ticker,
    standard_deviations_by_ticker=standard_deviations_by_ticker,
    ticker_encoder=ticker_encoder,
    dropout_rate=dropout_rate,
)

epoch_count = 50

training_losses = miniature_temporal_fusion_transformer_model.train(
    dataset=training_dataset,
    epoch_count=epoch_count,
    learning_rate=1e-3,
)

miniature_temporal_fusion_transformer_model.save()

Tensor.training = False

loaded_miniature_temporal_fusion_transformer_model = MiniatureTemporalFusionTransformer()

loaded_miniature_temporal_fusion_transformer_model.load()

prediction_data = pl.read_csv("platform/pricemodel/prediction_data.csv")

prediction_dataset = DataSet(
    batch_size=batch_size,
    sequence_length=sequence_length,
)

prediction_dataset.load_data(prediction_data)

predictions_p25, predictions_p50, predictions_p75 = (
    miniature_temporal_fusion_transformer_model.predict(prediction_dataset),
)

print(predictions_p25)
print(predictions_p50)
print(predictions_p75)
