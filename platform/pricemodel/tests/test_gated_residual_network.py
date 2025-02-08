from pricemodel.gated_residual_network import GatedResidualNetwork
from tinygrad import Tensor


def test_gated_residual_network():
    input_size = 16
    hidden_size = 32
    output_size = 16

    gated_residual_network = GatedResidualNetwork(
        input_size=input_size,
        hidden_size=hidden_size,
        output_size=output_size,
    )

    assert gated_residual_network.dense_input.weight.shape == (
        hidden_size,
        input_size,
    ), "Dense input weight shape mismatch"
    assert gated_residual_network.dense_input.bias.shape == (
        hidden_size,
    ), "Dense input bias shape mismatch"
    assert gated_residual_network.dense_output.weight.shape == (
        output_size,
        hidden_size,
    ), "Dense output weight shape mismatch"
    assert gated_residual_network.dense_output.bias.shape == (
        output_size,
    ), "Dense output bias shape mismatch"
    assert gated_residual_network.gate.weight.shape == (
        output_size,
        hidden_size,
    ), "Gate weight shape mismatch"
    assert gated_residual_network.gate.bias.shape == (output_size,), "Gate bias shape mismatch"
    assert gated_residual_network.layer_normalizer.weight.shape == (
        output_size,
    ), "layer normalizer weight shape mismatch"
    assert gated_residual_network.layer_normalizer.bias.shape == (
        output_size,
    ), "layer normalizer bias shape mismatch"

    input = Tensor.zeros(5, 16)  # output shape from lstm

    output = gated_residual_network.forward(input)

    assert output.shape == (5, 16), "Output shape mismatch"
