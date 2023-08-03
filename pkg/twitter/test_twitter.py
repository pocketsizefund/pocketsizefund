import unittest

from pkg.twitter import twitter


class MockTwitterClient:
    def __init__(
        self,
    ) -> None:
        pass

    def create_tweet(
        self,
        text: str,
    ) -> any:
        return None


class TestSendTweet(unittest.TestCase):
    def test_send_tweet_success(self):
        client = twitter.Client(
            api_key='api_key',
            api_key_secret='api_key_secret',
            access_token='access_token',
            access_token_secret='access_token_secret',
        )

        client.twitter_client = MockTwitterClient()

        client.send_tweet(
            text='text',
        )
