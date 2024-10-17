from tinygrad import nn
import sys

class TemporalFusionTransformer:
    def __init__(self, num_tickers: int, input_dim: int, embedding_dim: int):
        self.num_tickers = num_tickers
        self.embedding_dim = embedding_dim
        self.input_dim = input_dim
        self.ticker_embedding = nn.Embedding(self.num_tickers, self.embedding_dim)
        self.lstm = nn.LSTMCell(12, 8)
        # self.lstm2 = nn.LSTMCell(12, 12)
        self.fc1 = nn.Linear(8, 4, bias=True)

    def __call__(self, x):
        print(x.shape)
        sys.exit(1)
        x, _ = self.lstm(x)
        print(x.shape)

        sys.exit(1)
        # x, _ = self.lstm2(x)
        x = self.fc1(x)

        return x

