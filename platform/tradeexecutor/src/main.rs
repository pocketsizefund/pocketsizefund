use actix_web::middleware::Logger;
use actix_web::{post, web, App, HttpResponse, HttpServer};
use chrono::{DateTime, Utc};
use cloudevents::{Data, Event};
use mockall::mock;
use pocketsizefund::events::build_response_event;
use pocketsizefund::trade::{
    Client as TradeClient, Error as TradeError, Interface as TradeInterface, Order,
    PatternDayTraderCheck, PortfolioPerformance, PortfolioPosition,
    Side::{Long, Short},
};
use serde::Deserialize;
use std::env;
use std::io;
use std::num::ParseIntError;
use std::sync::Arc;

#[derive(Deserialize)]
struct Position {
    ticker: String,
    investment_amount: f64,
}

#[derive(Deserialize)]
struct PositionPayload {
    positions: Vec<Position>,
}

#[post("/health")]
async fn health_handler() -> HttpResponse {
    HttpResponse::Ok().body("OK")
}

#[post("/trade")]
async fn trade_handler(
    event: web::Json<Event>,
    trade_client: web::Data<Arc<dyn TradeInterface>>,
) -> Result<Event, Box<dyn std::error::Error>> {
    let mut new_positions: Vec<Position> = Vec::new();

    if let Some(Data::Json(json)) = event.data() {
        let payload: PositionPayload = match serde_json::from_value(json.clone()) {
            Ok(val) => val,
            Err(error) => {
                return Err(error.into());
            }
        };

        new_positions = payload.positions;
    }

    let mut closing_orders: Vec<Order> = Vec::new();
    let old_positions = trade_client.get_portfolio_positions().await?;
    for old_position in old_positions {
        let closing_order = Order {
            ticker: old_position.ticker,
            quantity: old_position.quantity,
            side: Short, // assumes long positions only
        };

        closing_orders.push(closing_order);
    }

    let total_investment_amount: f64 = new_positions.iter().map(|p| p.investment_amount).sum();

    let mut opening_orders: Vec<Order> = Vec::new();
    for new_position in new_positions {
        let opening_order = Order {
            ticker: new_position.ticker,
            quantity: (new_position.investment_amount / total_investment_amount).floor(),
            side: Long, // assumes long positions only
        };

        opening_orders.push(opening_order);
    }

    trade_client.execute_orders(opening_orders).await?;

    Ok(build_response_event(
        "tradeexecutor".to_string(),
        vec!["trades".to_string(), "completed".to_string()],
        None,
    ))
}

#[actix_web::main]
async fn main() -> std::io::Result<()> {
    env_logger::init_from_env(env_logger::Env::default().default_filter_or("info"));

    let server_port_environment_variable = env::var("SERVER_PORT").unwrap_or("8080".to_string());

    let server_port = server_port_environment_variable
        .parse::<u16>()
        .map_err(|e: ParseIntError| io::Error::new(io::ErrorKind::InvalidInput, e))?;

    let trade_client = TradeClient::new(
        env::var("ALPACA_API_KEY").expect("Alpaca API key"),
        env::var("ALPACA_API_SECRET").expect("Alpaca API secret"),
        env::var("DARQUBE_API_KEY").expect("Darqube API key"),
        env::var("ENVIRONMENT")
            .expect("Environment")
            .eq("production"),
    );

    let trade_client: Arc<dyn TradeInterface> = Arc::new(trade_client);

    let trade_client = web::Data::new(trade_client);

    HttpServer::new(move || {
        App::new()
            .wrap(Logger::default())
            .app_data(trade_client.clone())
            .service(health_handler)
            .service(trade_handler)
    })
    .bind(("0.0.0.0", server_port))?
    .run()
    .await
}

mock! {
    pub TradeInterfaceMock {}

    #[async_trait::async_trait]
    impl TradeInterface for TradeInterfaceMock {
        async fn get_available_tickers(&self) -> Result<Vec<String>, TradeError>;
        async fn get_portfolio_performance(&self, end_at: DateTime<Utc>) -> Result<PortfolioPerformance, TradeError>;
        async fn get_portfolio_positions(&self) -> Result<Vec<PortfolioPosition>, TradeError>;
        async fn check_orders_pattern_day_trade_restrictions(
            &self,
            orders: Vec<Order>,
        ) -> Result<Vec<PatternDayTraderCheck>, TradeError>;
        async fn execute_orders(&self, orders: Vec<Order>) -> Result<(), TradeError>;
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use actix_web::{http::header::ContentType, test, App};
    use serde_json::json;

    #[actix_web::test]
    async fn test_health_handler() {
        let app = test::init_service(App::new().service(health_handler)).await;

        let request = test::TestRequest::post()
            .uri("/health")
            .insert_header(ContentType::plaintext())
            .to_request();

        let response = test::call_service(&app, request).await;

        assert!(response.status().is_success());
    }

    #[actix_web::test]
    async fn test_trade_handler() {
        let mut mock_trade_client: MockTradeInterfaceMock = MockTradeInterfaceMock::new();

        mock_trade_client
            .expect_get_portfolio_positions()
            .returning(|| {
                Ok(vec![
                    PortfolioPosition {
                        ticker: "AAPL".to_string(),
                        side: Long,
                        quantity: 10.0,
                    },
                    PortfolioPosition {
                        ticker: "GOOGL".to_string(),
                        side: Long,
                        quantity: 20.0,
                    },
                ])
            });

        mock_trade_client
            .expect_execute_orders()
            .returning(|orders| {
                assert_eq!(orders.len(), 2);
                Ok(())
            });

        let mock_trade_client: Arc<dyn TradeInterface> = Arc::new(mock_trade_client);

        let mock_trade_client = web::Data::new(mock_trade_client);

        let app = test::init_service(
            App::new()
                .service(trade_handler)
                .app_data(mock_trade_client.clone()),
        )
        .await;

        env::set_var("ALPACA_API_KEY", "VALUE");
        env::set_var("ALPACA_API_SECRET", "VALUE");
        env::set_var("DARQUBE_API_KEY", "VALUE");
        env::set_var("ENVIRONMENT", "development");

        let request = test::TestRequest::post()
            .uri("/trade")
            .insert_header(ContentType::json())
            .set_json(&json!({
                "specversion": "1.0",
                "type": "metrics.get",
                "source": "pocketsizefund.positionmanager",
                "id": "1234",
                "time": "1997-05-25T20:00:00Z",
                "data": {
                    "positions": [
                        {
                            "ticker": "AAPL",
                            "investment_amount": 100.0
                        },
                        {
                            "ticker": "GOOGL",
                            "investment_amount": 200.0
                        }
                    ]
                }
            }))
            .to_request();

        let response = test::call_service(&app, request).await;

        assert!(response.status().is_success());
    }
}
