import unittest

from pkg.twitter import twitter


class MockTwitterClient:
    def __init__(self) -> None:
        pass

    def create_tweet(
        self,
        text: str,
    ) -> any:
        return None


class MockTwitterAPI:
    def __init__(self) -> None:
        pass

    def update_profile(
        self,
        location: str,
        description: str,
        profile_link_color: str,
    ) -> None:
        _ = location, description, profile_link_color

        return None

    def update_profile_banner(
        self,
        filename: str,
    ) -> None:
        _ = filename

        return None

    def update_profile_image(
        self,
        filename: str,
    ) -> None:
        _ = filename

        return None


class TestSendTweet(unittest.TestCase):
    def test_send_tweet_success(self) -> None:
        client = twitter.Client(
            api_key="api_key",
            api_key_secret="api_key_secret",
            access_token="access_token",
            access_token_secret="access_token_secret",
            image_files_path="image/files/path",
        )

        client.twitter_client = MockTwitterClient()

        client.send_tweet(
            text="text",
        )


class TestUpdateProfile(unittest.TestCase):
    def test_update_profile_success(self) -> None:
        client = twitter.Client(
            api_key="api_key",
            api_key_secret="api_key_secret",
            access_token="access_token",
            access_token_secret="access_token_secret",
            image_files_path="images",
        )

        client.twitter_api = MockTwitterAPI()

        client.update_profile(twitter.configuration_orange)
