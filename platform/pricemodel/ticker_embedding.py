from tinygrad.nn import Embedding
from tinygrad import Tensor


class TickerEmbedding:
    def __init__(self, ticker_count: int, embedding_size: int):
        self.embedding = Embedding(ticker_count, embedding_size)

    # NOTE: add type annotation for output
    def forward(self, tickers: Tensor):
        output = self.embedding(tickers).realize()  # keep realize

        output = output.realize()

        return output
