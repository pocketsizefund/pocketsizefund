pub mod config;
pub mod routes;
use crate::config::DiscordWebhook;
use actix_web::dev::Server;
use actix_web::middleware::Logger;
use actix_web::{web, App, HttpServer};
use std::net::TcpListener;
use tracing::debug;

pub fn run(listener: TcpListener) -> Result<Server, std::io::Error> {
    env_logger::init_from_env(env_logger::Env::default().default_filter_or("info"));

    let discord_webhook = DiscordWebhook::from_env();

    debug!("using discord webhook: {}", discord_webhook.url);

    let server = HttpServer::new(move || {
        App::new()
            .wrap(Logger::default())
            .app_data(web::Data::new(discord_webhook.clone()))
            .service(routes::health::check)
            .service(routes::message::event_handler)
    })
    .listen(listener)?
    .run();

    Ok(server)
}
