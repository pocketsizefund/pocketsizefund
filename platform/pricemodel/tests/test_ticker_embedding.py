from pricemodel.ticker_embedding import TickerEmbedding
from tinygrad import Tensor


def test_ticker_embedding():
    ticker_embedding = TickerEmbedding(10, 3)
    tickers = Tensor([1, 2, 3])
    embedding = ticker_embedding.forward(tickers)
    assert embedding.shape == (3, 3)
    assert embedding[0].shape == (3,)
    assert embedding[1].shape == (3,)
    assert embedding[2].shape == (3,)
    assert embedding[0].tolist() == ticker_embedding.embedding.weight[1].tolist()
    assert embedding[1].tolist() == ticker_embedding.embedding.weight[2].tolist()
    assert embedding[2].tolist() == ticker_embedding.embedding.weight[3].tolist()
    assert embedding.tolist() == ticker_embedding.embedding.weight[1:4].tolist()
    assert embedding.tolist() == ticker_embedding.embedding(tickers).tolist()
