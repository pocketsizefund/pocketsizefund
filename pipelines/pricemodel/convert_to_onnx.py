import torch
from pytorch_forecasting import TemporalFusionTransformer

model = TemporalFusionTransformer.load_from_checkpoint("tft-model-checkpoint.ckpt")

model.eval()


batch_size = 1
sequence_length = 10
num_features = 10  # adjust based on your model

x = torch.randn(batch_size, sequence_length, num_features)

torch.onnx.export(
    model,
    x,
    "tft_model.onnx",
    export_params=True,
    opset_version=11,
    do_constant_folding=True,
    input_names=["input"],
    output_names=["output"],
    dynamic_axes={
        "input": {0: "batch_size", 1: "sequence"},
        "output": {0: "batch_size", 1: "sequence"},
    },
)
