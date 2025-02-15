from tinygrad import nn, dtypes, Tensor, TinyJit, GlobalCounters


class SimpleModel:
    def __init__(self, input_size: int, hidden_size: int = 1):
        self.input_size = input_size
        self.hidden_size = hidden_size
        self.fc = nn.Linear(self.input_size, self.hidden_size)

    def __call__(self, x: Tensor) -> Tensor:
        return self.fc(x)
