import unittest

from pkg.twitter import twitter

API_KEY = "api_key"
API_KEY_SECRET = "api_key_secret"  # noqa: S106
ACCESS_TOKEN = "access_token"  # noqa: S106
ACCESS_TOKEN_SECRET = "access_token_secret"  # noqa: S106


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

    def update_profile_banner(
        self,
        filename: str,
    ) -> None:
        _ = filename

    def update_profile_image(
        self,
        filename: str,
    ) -> None:
        _ = filename


class TestSendTweet(unittest.TestCase):
    def test_send_tweet_success(self) -> None:
        client = twitter.Client(
            api_key=API_KEY,
            api_key_secret=API_KEY_SECRET,
            access_token=ACCESS_TOKEN,
            access_token_secret=ACCESS_TOKEN_SECRET,
            image_files_path="image/files/path",
        )

        client.twitter_client = MockTwitterClient()

        client.send_tweet(
            text="text",
        )


class TestUpdateProfile(unittest.TestCase):
    def test_update_profile_success(self) -> None:
        client = twitter.Client(
            api_key=API_KEY,
            api_key_secret=API_KEY_SECRET,
            access_token=ACCESS_TOKEN,
            access_token_secret=ACCESS_TOKEN_SECRET,
            image_files_path="images",
        )

        client.twitter_api = MockTwitterAPI()

        client.update_profile(twitter.configuration_orange)
