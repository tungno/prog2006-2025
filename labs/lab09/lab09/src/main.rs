use axum::{
    routing::{get, post},
    Router,
};
use rest_api_lab::{
    not_found, hello_world, greet_name, greet_me, get_announcements
};
use std::net::SocketAddr;

#[tokio::main]
async fn main() {
    // Build our application with routes
    let app = Router::new()
        .route("/", get(not_found))
        .route("/hello", get(hello_world))
        .route("/greet/:name", get(greet_name))
        .route("/greetme", post(greet_me))
        .route("/announcements", get(get_announcements));

    // Run it with hyper on localhost:3000
    let addr = SocketAddr::from(([127, 0, 0, 1], 3000));
    println!("Server running on http://{}", addr);
    axum::Server::bind(&addr)
        .serve(app.into_make_service())
        .await
        .unwrap();
}