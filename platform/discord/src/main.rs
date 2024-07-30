use std::net::TcpListener;

use discord::run;

#[tokio::main]
async fn main() -> Result<(), std::io::Error> {
    run(TcpListener::bind("0.0.0.0:8080").expect("failed to bind to port"))?.await
}
