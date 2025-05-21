use iced::keyboard::KeyCode;
use crate::logic::snake::Direction;

/// Converts a key code to a direction for the first player (WASD)
pub fn key_to_direction_p1(key: KeyCode) -> Option<Direction> {
    match key {
        KeyCode::W => {
            println!("Player 1 pressed W (Up)");
            Some(Direction::Up)
        },
        KeyCode::A => {
            println!("Player 1 pressed A (Left)");
            Some(Direction::Left)
        },
        KeyCode::S => {
            println!("Player 1 pressed S (Down)");
            Some(Direction::Down)
        },
        KeyCode::D => {
            println!("Player 1 pressed D (Right)");
            Some(Direction::Right)
        },
        _ => None,
    }
}

/// Converts a key code to a direction for the second player (Arrow keys)
pub fn key_to_direction_p2(key: KeyCode) -> Option<Direction> {
    match key {
        KeyCode::Up => {
            println!("Player 2 pressed Up Arrow");
            Some(Direction::Up)
        },
        KeyCode::Left => {
            println!("Player 2 pressed Left Arrow");
            Some(Direction::Left)
        },
        KeyCode::Down => {
            println!("Player 2 pressed Down Arrow");
            Some(Direction::Down)
        },
        KeyCode::Right => {
            println!("Player 2 pressed Right Arrow");
            Some(Direction::Right)
        },
        _ => None,
    }
}

/// Converts grid coordinates to pixel coordinates for rendering
pub fn grid_to_pixels(x: i32, y: i32, cell_size: f32) -> (f32, f32) {
    (x as f32 * cell_size, y as f32 * cell_size)
}

/// Converts a score to a formatted string
pub fn format_score(score: u32) -> String {
    format!("Score: {}", score)
}