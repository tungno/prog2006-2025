#[cfg(test)]
mod tests {
    use axum::{
        body::Body,
        http::{Method, Request, StatusCode},
        routing::{get, post},
        Router,
    };
    use rest_api_lab::{hello_world, greet_name, greet_me};
    use serde_json::{json, Value};
    use tower::ServiceExt;

    fn app() -> Router {
        Router::new()
            .route("/hello", get(hello_world))
            .route("/greet/:name", get(greet_name))
            .route("/greetme", post(greet_me))
    }

    #[tokio::test]
    async fn test_hello_world() {
        let app = app();

        let response = app
            .oneshot(Request::builder().uri("/hello").body(Body::empty()).unwrap())
            .await
            .unwrap();

        assert_eq!(response.status(), StatusCode::OK);
        
        let body = hyper::body::to_bytes(response.into_body()).await.unwrap();
        assert_eq!(&body[..], b"Hello, World!");
    }

    #[tokio::test]
    async fn test_greet_name() {
        let app = app();

        let response = app
            .oneshot(Request::builder().uri("/greet/Mariusz").body(Body::empty()).unwrap())
            .await
            .unwrap();

        assert_eq!(response.status(), StatusCode::OK);
        
        let body = hyper::body::to_bytes(response.into_body()).await.unwrap();
        let json: Value = serde_json::from_slice(&body).unwrap();
        
        assert_eq!(json["greet"], "Hello");
        assert_eq!(json["name"], "Mariusz");
    }

    #[tokio::test]
    async fn test_greet_me() {
        let app = app();
        
        let json_payload = json!({
            "input": "whatever text",
            "name": "Mariusz"
        });

        let response = app
            .oneshot(
                Request::builder()
                    .method(Method::POST)
                    .uri("/greetme")
                    .header("Content-Type", "application/json")
                    .body(Body::from(serde_json::to_vec(&json_payload).unwrap()))
                    .unwrap(),
            )
            .await
            .unwrap();

        assert_eq!(response.status(), StatusCode::OK);
        
        let body = hyper::body::to_bytes(response.into_body()).await.unwrap();
        let json: Value = serde_json::from_slice(&body).unwrap();
        
        assert_eq!(json["msg"], "whatever text Mariusz");
    }
}