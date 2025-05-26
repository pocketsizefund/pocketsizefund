from tinygrad.nn import Embedding
from tinygrad.tensor import Tensor


class TickerEmbedding:
    def __init__(self, ticker_count: int, embedding_size: int):
        self.embedding = Embedding(ticker_count, embedding_size)

    def forward(self, tickers: Tensor):
        return self.embedding(tickers)
