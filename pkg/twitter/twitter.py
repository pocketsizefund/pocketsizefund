import tweepy


class Client:
    def __init__(
        self,
        api_key: str,
        api_key_secret: str,
        access_token: str,
        access_token_secret: str,
    ) -> None:
        self.twitter_client = tweepy.Client(
            consumer_key=api_key,
            consumer_secret=api_key_secret,
            access_token=access_token,
            access_token_secret=access_token_secret,
        )

    def send_tweet(
        self,
        text: str,
    ) -> None:
        self.twitter_client.create_tweet(text=text)
