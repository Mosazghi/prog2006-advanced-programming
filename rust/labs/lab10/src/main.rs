use axum::{
    http::StatusCode,
    routing::{get, post},
    Json, Router,
};
use serde::{Deserialize, Serialize};

#[derive(Deserialize, Serialize, Debug)]
struct Greet {
    greet: String,
    name: String,
}
#[tokio::main]
async fn main() {
    let app = Router::new()
        .route("/hello", get(hello))
        .route("/", get(not_found))
        .route("/greet", get(greet))
        .route("/greetme", post(greetme));

    let listener = tokio::net::TcpListener::bind("127.0.0.1:3000")
        .await
        // The use of `unwrap_or_else` might be unnecessary.
        .unwrap_or_else(|_| {
            print!("error, could not bind to port 3000");
            std::process::exit(1);
        });

    println!("Listening on: https://{}", listener.local_addr().unwrap());
    // The use of `unwrap_or_else` might be unnecessary.
    axum::serve(listener, app).await.unwrap_or_else(|_| {
        print!("error, could not start the server");
        std::process::exit(1);
    });
}

async fn hello() -> (StatusCode, &'static str) {
    (StatusCode::OK, "Hello, World!")
}
async fn not_found() -> (StatusCode, &'static str) {
    (StatusCode::NOT_FOUND, "404 page not found")
}

async fn greet() -> (StatusCode, Json<Greet>) {
    println!("[GET]");

    (
        StatusCode::OK,
        Json(Greet {
            greet: "Hello".to_string(),
            name: "Mariusz".to_string(),
        }),
    )
}

async fn greetme(payload: Option<Json<Greet>>) -> (StatusCode, Result<Json<Greet>, String>) {
    /*
     * Here we check if the payload (request) is of type `Greet` and return the appropriate response
     */
    match payload {
        Some(greet) => {
            println!("[POST] {:#?}", greet);
            (StatusCode::OK, Ok(greet))
        }
        None => {
            let err = Err("Incorrect payload".to_string());
            println!("[FAILED POST] {:#?}", err);
            (StatusCode::BAD_REQUEST, err)
        }
    }
}
