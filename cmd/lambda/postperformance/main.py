import os
import datetime

from pkg.trade import trade
from pkg.twitter import twitter


trade_client = trade.Client(
    darqube_api_key=os.getenv("DARQUBE_API_KEY"),
    alpaca_api_key=os.getenv("ALPACA_API_KEY"),
    alpaca_api_secret=os.getenv("ALPACA_API_SECRET"),
    alpha_vantage_api_key=os.getenv("ALPHA_VANTAGE_API_KEY"),
    is_paper=True if os.getenv("IS_PAPER") == "true" else False,
)

twitter_client = twitter.Client(
    api_key=os.getenv("TWITTER_API_KEY"),
    api_key_secret=os.getenv("TWITTER_API_KEY_SECRET"),
    access_token=os.getenv("TWITTER_ACCESS_TOKEN"),
    access_token_secret=os.getenv("TWITTER_ACCESS_TOKEN_SECRET"),
    image_files_path=os.getenv("IMAGE_FILES_PATH"),
)


def handler(
    event: any,
    context: any,
) -> dict[str, any]:
    _ = event, context

    text = '''
Performance metrics past {} weeks

Portfolio cumulative returns: {}
Benchmark cumulative returns: {}
'''

    week_count = 2

    performance_metrics = trade_client.get_performance_metrics(
        week_count=week_count,
        end_at=datetime.datetime.now(),
    )

    text = text.format(
        week_count,
        performance_metrics["cumulative_portfolio_returns"],
        performance_metrics["cumulative_benchmark_returns"],
    )

    twitter_client.send_tweet(
        text=text,
    )
