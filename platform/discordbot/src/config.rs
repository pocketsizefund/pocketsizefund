use url::Url;

#[derive(Clone)]
pub struct DiscordWebhook {
    pub url: Url,
}

impl DiscordWebhook {
    pub fn from_env() -> Self {
        let webhook_url =
            std::env::var("DISCORD_WEBHOOK_URL").expect("Discord webhook URL must be set");
        let url = Url::parse(&webhook_url).expect("Invalid Discord webhook URL");
        Self { url }
    }
}
