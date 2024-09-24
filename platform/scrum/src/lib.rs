pub mod routes;
use actix_web::dev::Server;
use actix_web::middleware::Logger;
use actix_web::{App, HttpServer};
use anyhow::Result;
use std::net::TcpListener;

pub fn run(listener: TcpListener) -> Result<Server> {
    env_logger::init_from_env(env_logger::Env::default().default_filter_or("info"));

    let server = HttpServer::new(move || {
        App::new()
            .wrap(Logger::default())
            .service(routes::health::check)
            .service(routes::issues::handler)
    })
    .listen(listener)?
    .run();

    Ok(server)
}
