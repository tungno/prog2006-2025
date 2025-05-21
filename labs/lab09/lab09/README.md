# REST API Lab: Course Announcements Service

This project implements a RESTful API server using Rust and the Axum framework that serves both basic endpoints and a more complex course announcements service with HTML and JSON response formats.

## Project Overview

The application provides several API endpoints:

1. **Basic Hello World**: A simple greeting endpoint
2. **Dynamic Greeting**: An endpoint that greets users by name
3. **Greeting Post**: A POST endpoint that accepts JSON data for personalized greetings
4. **Course Announcements**: An endpoint that fetches, sorts, and displays course announcements from GitLab in either HTML or JSON format

The project demonstrates:
- REST API design principles
- Asynchronous programming in Rust
- JSON processing with Serde
- External API integration (GitLab API)
- Content negotiation (HTML vs. JSON responses)
- Error handling in web services

## Prerequisites

- [Rust](https://www.rust-lang.org/tools/install) (2021 edition or newer)
- [Cargo](https://doc.rust-lang.org/cargo/getting-started/installation.html) (comes with Rust)
- Internet connection (for fetching GitLab announcements)

## Building and Running

1. Clone the repository:
   ```bash
   git clone <repository-url>
   cd lab09
   ```

2. Build the project:
   ```bash
   cargo build
   ```

3. Run the server:
   ```bash
   cargo run
   ```

The server will be available at `http://127.0.0.1:3000`

## API Endpoints

### 1. Default Route

- **URL**: `/`
- **Method**: GET
- **Response**: 404 Not Found
- **Description**: Demonstrates basic error handling

### 2. Hello World

- **URL**: `/hello`
- **Method**: GET
- **Response**: Plain text "Hello, World!"
- **Description**: Simple greeting endpoint

Example:
```bash
curl http://localhost:3000/hello
```

### 3. Parameterized Greeting

- **URL**: `/greet/:name`
- **Method**: GET
- **URL Parameters**: `name` - The name to greet
- **Response**: JSON object with greeting
- **Description**: Demonstrates URL parameter extraction

Example:
```bash
curl http://localhost:3000/greet/Mariusz
```

Response:
```json
{"greet":"Hello","name":"Mariusz"}
```

### 4. JSON POST Endpoint

- **URL**: `/greetme`
- **Method**: POST
- **Request Body**: JSON with `input` and `name` fields
- **Response**: JSON with concatenated message
- **Description**: Demonstrates handling JSON POST requests

Example:
```bash
curl -X POST http://localhost:3000/greetme \
  -H "Content-Type: application/json" \
  -d '{"input":"whatever text","name":"Mariusz"}'
```

Response:
```json
{"msg":"whatever text Mariusz"}
```

### 5. Course Announcements

- **URL**: `/announcements`
- **Method**: GET
- **Query Parameters**: `format` - Optional parameter to specify response format (json/html)
- **Response**: 
  - HTML page with formatted announcements (default)
  - JSON array of announcements (when format=json)
- **Description**: Fetches course announcements from GitLab API, sorts them by date, and returns them in the requested format

Example JSON:
```bash
curl http://localhost:3000/announcements?format=json
```

Example HTML:
```bash
curl http://localhost:3000/announcements
# Or open in browser: http://localhost:3000/announcements
```

## Implementation Details

### Project Structure

- **main.rs**: Entry point that sets up the Axum router and server
- **lib.rs**: Core implementation of handlers and data structures
- **tests/api_tests.rs**: Test cases for API endpoints

### Key Components

1. **Route Handlers**:
   - `not_found`: Returns a 404 status
   - `hello_world`: Returns a simple greeting
   - `greet_name`: Extracts name from URL path and returns JSON
   - `greet_me`: Processes JSON request and returns response
   - `get_announcements`: Fetches and formats course announcements

2. **Data Models**:
   - `GreetResponse`: Structure for name greeting response
   - `GreetMeRequest`: Structure for parsing incoming JSON
   - `GreetMeResponse`: Structure for formatted greeting response
   - `Announcement`: Structure representing a course announcement

3. **External API Integration**:
   - `fetch_announcements`: Retrieves announcements from GitLab API
   - `generate_html_response`: Formats announcements as HTML

### Technologies Used

- **Axum**: Modern Rust web framework for building async services
- **Tokio**: Async runtime for Rust
- **Serde**: Serialization/deserialization framework
- **Reqwest**: HTTP client for API requests
- **Chrono**: Date and time handling

## Testing

The project includes automated tests for the API endpoints. Run them with:

```bash
cargo test
```

The tests verify:
- The hello endpoint returns the correct greeting
- The parameterized greeting endpoint works properly
- The POST endpoint correctly processes JSON and returns the expected response

## Error Handling

The application incorporates several error handling mechanisms:

- Default 404 route for non-existent endpoints
- Request parsing errors handled via Axum's extraction mechanism
- Graceful handling of GitLab API failures
- Fallback date parsing for malformed announcement dates

## Future Enhancements

Potential improvements for the project:

1. **Authentication**: Add user authentication for protected endpoints
2. **Caching**: Implement caching for GitLab API responses
3. **API Documentation**: Add Swagger/OpenAPI documentation
4. **More Endpoints**: Support additional course information endpoints
5. **Logging**: Add structured logging for better debugging

## License

This project is licensed under the MIT License - see the LICENSE file for details.