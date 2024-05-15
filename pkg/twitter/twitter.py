import tweepy

configuration_orange = "orange"
configuration_blueberry = "blueberry"
configuration_banana = "banana"
configuration_apple = "apple"
configuration_strawberry = "strawberry"


class Client:
    def __init__(
        self,
        api_key: str,
        api_key_secret: str,
        access_token: str,
        access_token_secret: str,
        image_files_path: str,
    ) -> None:
        self.twitter_client = tweepy.Client(
            consumer_key=api_key,
            consumer_secret=api_key_secret,
            access_token=access_token,
            access_token_secret=access_token_secret,
        )

        auth = tweepy.OAuth1UserHandler(
            consumer_key=api_key,
            consumer_secret=api_key_secret,
            access_token=access_token,
            access_token_secret=access_token_secret,
        )

        self.twitter_api = tweepy.API(auth)

        self.fruit = {
            configuration_orange: "🍊",
            configuration_blueberry: "🫐",
            configuration_banana: "🍌",
            configuration_apple: "🍏",
            configuration_strawberry: "🍓",
        }

        self.colors = {
            configuration_orange: "FF9A00",
            configuration_blueberry: "4A86E8",
            configuration_banana: "F1E032",
            configuration_apple: "81CB61",
            configuration_strawberry: "CC0000",
        }

        self.banners = {
            configuration_orange: image_files_path + "/orange_banner.jpg",
            configuration_blueberry: image_files_path + "/blueberry_banner.jpg",
            configuration_banana: image_files_path + "/banana_banner.jpg",
            configuration_apple: image_files_path + "/apple_banner.jpg",
            configuration_strawberry: image_files_path + "/strawberry_banner.jpg",
        }

        self.images = {
            configuration_orange: image_files_path + "/orange_profile.png",
            configuration_blueberry: image_files_path + "/blueberry_profile.png",
            configuration_banana: image_files_path + "/banana_profile.png",
            configuration_apple: image_files_path + "/apple_profile.png",
            configuration_strawberry: image_files_path + "/strawberry_profile.png",
        }

    def send_tweet(
        self,
        text: str,
    ) -> None:
        self.twitter_client.create_tweet(text=text)

    def update_profile(
        self,
        configuration: str,
    ) -> None:
        self.twitter_api.update_profile(
            location=self.fruit[configuration],
            description="Recreational quantitative trading " +
            self.fruit[configuration],
            profile_link_color=self.colors[configuration],
        )

        self.twitter_api.update_profile_banner(
            filename=self.banners[configuration],
        )

        self.twitter_api.update_profile_image(
            filename=self.images[configuration],
        )
