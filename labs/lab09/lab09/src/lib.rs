use axum::{
    extract::{Path, Query},
    http::StatusCode,
    response::{Html, IntoResponse, Response},
    Json,
};
use chrono::{DateTime, Utc};
use serde::{Deserialize, Serialize};
use std::collections::HashMap;

// Handler for the default route - returns 404
pub async fn not_found() -> (StatusCode, &'static str) {
    (StatusCode::NOT_FOUND, "404 Page not found")
}

// Handler for /hello - returns simple text
pub async fn hello_world() -> &'static str {
    "Hello, World!"
}

// Handler for /greet/:name - extracts name from URL and returns JSON
pub async fn greet_name(Path(name): Path<String>) -> Json<GreetResponse> {
    let response = GreetResponse {
        greet: "Hello".to_string(),
        name: name,
    };
    Json(response)
}

// Handler for POST /greetme - accepts JSON input and returns JSON response
pub async fn greet_me(Json(payload): Json<GreetMeRequest>) -> Json<GreetMeResponse> {
    let response = GreetMeResponse {
        msg: format!("{} {}", payload.input, payload.name),
    };
    Json(response)
}

// Structs for JSON serialization/deserialization
#[derive(Serialize)]
pub struct GreetResponse {
    pub greet: String,
    pub name: String,
}

#[derive(Deserialize)]
pub struct GreetMeRequest {
    pub input: String,
    pub name: String,
}

#[derive(Serialize)]
pub struct GreetMeResponse {
    pub msg: String,
}

// Query parameters for the announcements endpoint
#[derive(Deserialize)]
pub struct AnnouncementParams {
    pub format: Option<String>,
}

// Announcement data structures
#[derive(Debug, Deserialize, Serialize)]
pub struct Announcement {
    pub id: u64,
    pub title: String,
    pub description: String,
    pub created_at: String,
    pub updated_at: String,
    // Include other fields as needed
}

// Handler for /announcements - fetches announcements and returns HTML or JSON
pub async fn get_announcements(Query(params): Query<AnnouncementParams>) -> impl IntoResponse {
    // Fetch announcements from the GitLab API
    let announcements = fetch_announcements().await.unwrap_or_else(|_| vec![]);
    
    // Sort announcements by created_at date
    let mut sorted_announcements = announcements;
    sorted_announcements.sort_by(|a, b| {
        let date_a = DateTime::parse_from_rfc3339(&a.created_at).unwrap_or_else(|_| DateTime::<Utc>::default().into());
        let date_b = DateTime::parse_from_rfc3339(&b.created_at).unwrap_or_else(|_| DateTime::<Utc>::default().into());
        date_a.cmp(&date_b)
    });

    // Check the format parameter to determine how to respond
    match params.format.as_deref() {
        Some("json") => Json(sorted_announcements).into_response(),
        _ => generate_html_response(&sorted_announcements).into_response(),
    }
}

// Function to generate HTML from announcements
pub fn generate_html_response(announcements: &[Announcement]) -> Html<String> {
    let mut html = String::from(
        "<!DOCTYPE html>\n\
        <html>\n\
        <head>\n\
            <title>Course Announcements</title>\n\
            <style>\n\
                body { font-family: Arial, sans-serif; margin: 0 auto; max-width: 800px; padding: 20px; }\n\
                h1 { color: #2c3e50; }\n\
                .announcement { border-bottom: 1px solid #eee; padding: 15px 0; }\n\
                .title { font-size: 18px; font-weight: bold; color: #3498db; }\n\
                .date { color: #7f8c8d; font-size: 14px; margin: 5px 0; }\n\
                .content { margin-top: 10px; }\n\
            </style>\n\
        </head>\n\
        <body>\n\
            <h1>Course Announcements</h1>\n",
    );

    if announcements.is_empty() {
        html.push_str("<p>No announcements available.</p>");
    } else {
        for announcement in announcements {
            html.push_str(&format!(
                "<div class=\"announcement\">\n\
                    <div class=\"title\">{}</div>\n\
                    <div class=\"date\">Created: {}</div>\n\
                    <div class=\"content\">{}</div>\n\
                </div>\n",
                announcement.title,
                announcement.created_at,
                announcement.description
            ));
        }
    }

    html.push_str(
        "</body>\n\
        </html>",
    );

    Html(html)
}

// Function to fetch announcements from the GitLab API
pub async fn fetch_announcements() -> Result<Vec<Announcement>, reqwest::Error> {
    let client = reqwest::Client::new();
    let response = client
        .get("https://git.gvk.idi.ntnu.no/api/v4/projects/5881/issues")
        .query(&[("labels", "Announcement"), ("state", "opened")])
        .send()
        .await?;

    let announcements = response.json::<Vec<Announcement>>().await?;
    Ok(announcements)
}