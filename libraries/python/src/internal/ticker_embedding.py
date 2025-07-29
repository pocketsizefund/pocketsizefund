from tinygrad.nn import Embedding
from tinygrad.tensor import Tensor


class TickerEmbedding:
    def __init__(self, ticker_count: int, embedding_size: int) -> None:
        self.embedding = Embedding(ticker_count, embedding_size)

    def forward(self, tickers: Tensor) -> Tensor:
        return self.embedding(tickers)
