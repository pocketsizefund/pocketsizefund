from tinygrad import Tensor
from pricemodel.gated_residual_network import (
    AddNorm,
    GatedLinearUnit,
    GateAddNorm,
    GatedResidualNetwork,
)


def test_add_norm():
    input_size = 4
    skip_size = 3

    class MockLayerNorm:
        def __init__(self, size):
            self.size = size

        def __call__(self, x: Tensor) -> Tensor:
            return x - x.mean(axis=-1, keepdims=True)

    class MockTimeDistributedInterpolation:
        def __init__(self, size):
            self.size = size

        def forward(self, x: Tensor) -> Tensor:
            return Tensor.linspace(0, 1, self.size).unsqueeze(0).repeat(x.shape[0], 1)

    AddNorm.TimeDistributedInterpolation = MockTimeDistributedInterpolation
    AddNorm.LayerNorm = MockLayerNorm

    add_norm = AddNorm(input_size=input_size, skip_size=skip_size)

    assert add_norm.input_size == input_size, "Incorrect input size initialization"
    assert add_norm.skip_size == skip_size, "Incorrect skip size initialization"
    assert hasattr(add_norm, "resampler"), "Resampler should be initialized when sizes differ"

    x = Tensor([[1.0, 2.0, 3.0, 4.0], [4.0, 5.0, 6.0, 7.0]])
    skip = Tensor([[1.0, 2.0, 3.0], [4.0, 5.0, 6.0]])

    output = add_norm.forward(x, skip)
    assert output.shape == x.shape, "Output shape mismatch"
    assert (output.mean(axis=-1).abs() < 1e-5).all().item(), "Normalization did not zero mean"

    add_norm_no_resample = AddNorm(input_size=input_size, skip_size=input_size)
    skip_no_resample = Tensor([[1.0, 2.0, 3.0, 4.0], [4.0, 5.0, 6.0, 7.0]])

    output_no_resample = add_norm_no_resample.forward(x, skip_no_resample)
    assert output_no_resample.shape == x.shape, "Output shape mismatch without resampling"
    assert (
        (output_no_resample.mean(axis=-1).abs() < 1e-5).all().item()
    ), "Normalization did not zero mean without resampling"


def test_gated_linear_unit():
    input_size = 4
    hidden_size = 6
    dropout_rate = 0.5

    class MockLinear:
        def __init__(self, in_features, out_features):
            self.in_features = in_features
            self.out_features = out_features

        def __call__(self, x: Tensor) -> Tensor:
            batch_size = x.shape[0]
            return Tensor.full((batch_size, self.out_features), 1.0)

    GatedLinearUnit.Linear = MockLinear

    gated_linear_unit = GatedLinearUnit(
        input_size=input_size, hidden_size=hidden_size, dropout_rate=dropout_rate
    )

    assert gated_linear_unit.fc.weight.shape == (
        hidden_size * 2,
        input_size,
    ), "Incorrect input size for Linear"
    assert gated_linear_unit.fc.bias.shape == (hidden_size * 2,), "Incorrect output size for Linear"

    x = Tensor([[1.0, 2.0, 3.0, 4.0], [4.0, 5.0, 6.0, 7.0]])  # Shape: (2, 4)
    output = gated_linear_unit.forward(x)

    assert output.shape == (2, hidden_size), "Output shape mismatch"

    gated_linear_unit_no_dropout = GatedLinearUnit(
        input_size=input_size, hidden_size=hidden_size, dropout_rate=None
    )
    output_no_dropout = gated_linear_unit_no_dropout.forward(x)

    assert output_no_dropout.shape == (2, hidden_size), "Output shape mismatch without dropout"


def test_gate_add_norm():
    class MockGatedLinearUnit:
        def __init__(self, input_size, hidden_size, dropout_rate):
            self.input_size = input_size
            self.hidden_size = hidden_size
            self.dropout_rate = dropout_rate

        def forward(self, x: Tensor) -> Tensor:
            return x * 2

    class MockAddNorm:
        def __init__(self, input_size, skip_size):
            self.input_size = input_size
            self.skip_size = skip_size

        def forward(self, x: Tensor, skip: Tensor) -> Tensor:
            return x + skip

    GateAddNorm.GatedLinearUnit = MockGatedLinearUnit
    GateAddNorm.AddNorm = MockAddNorm

    input_size = 4
    hidden_size = 6
    skip_size = 6
    dropout_rate = 0.5

    gate_add_norm = GateAddNorm(
        input_size=input_size,
        hidden_size=hidden_size,
        skip_size=skip_size,
        dropout_rate=dropout_rate,
    )

    x = Tensor([[1.0, 2.0, 3.0, 4.0]])
    skip = Tensor([[0.5, 1.0, 1.5, 2.0, 2.5, 3.0]])

    output = gate_add_norm.forward(x, skip)

    assert output.shape == (1, 6)


def test_gated_residual_network():
    class MockGateAddNorm:
        def __init__(self, input_size, skip_size, hidden_size, dropout_rate):
            self.input_size = input_size
            self.skip_size = skip_size
            self.hidden_size = hidden_size
            self.dropout_rate = dropout_rate

        def forward(self, x: Tensor, skip: Tensor) -> Tensor:
            return x + skip

    GatedResidualNetwork.GateAddNorm = MockGateAddNorm

    input_size = 4
    hidden_size = 6
    output_size = 4
    context_size = 3
    dropout_rate = 0.1

    grn = GatedResidualNetwork(
        input_size=input_size,
        hidden_size=hidden_size,
        output_size=output_size,
        dropout_rate=dropout_rate,
        context_size=context_size,
    )

    x = Tensor([[1.0, 2.0, 3.0, 4.0]])
    context = Tensor([[0.5, 1.0, 1.5]])

    output = grn.forward(x, context)

    assert output.shape == (1, output_size), "Output shape mismatch"
