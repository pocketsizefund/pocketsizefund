pub mod routes;
use actix_web::dev::Server;
use actix_web::middleware::Logger;
use actix_web::{web, App, HttpServer};
use apca::{ApiInfo, Client};

#[derive(Debug)]
pub struct AppState {
    pub alpaca_client: Client,
}

use std::net::TcpListener;

pub fn run(listener: TcpListener) -> Result<Server, std::io::Error> {
    env_logger::init_from_env(env_logger::Env::default().default_filter_or("info"));

    let api_info = match ApiInfo::from_env() {
        Ok(api_info) => api_info,
        Err(e) => {
            panic!("Failed to get API info: {:?}", e);
        }
    };
    let alpaca_client = Client::new(api_info);
    let app_state = web::Data::new(AppState { alpaca_client });

    let server = HttpServer::new(move || {
        App::new()
            .wrap(Logger::default())
            .app_data(app_state.clone())
            .service(routes::health::check)
            .service(routes::event::market_status_check)
    })
    .listen(listener)?
    .run();

    Ok(server)
}
