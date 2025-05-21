/// Size of the game grid (both width and height)
pub const GRID_SIZE: i32 = 20;

/// Size of each grid cell in pixels
pub const CELL_SIZE: f32 = 25.0;

/// Initial length of the snake
pub const INITIAL_SNAKE_LENGTH: usize = 3;

/// Game update speed in milliseconds
pub const GAME_SPEED: u64 = 150;

/// Color for the first player's snake (green)
pub const SNAKE1_COLOR: iced::Color = iced::Color {
    r: 0.0,
    g: 0.8,
    b: 0.0,
    a: 1.0,
};

/// Color for the second player's snake (blue)
pub const SNAKE2_COLOR: iced::Color = iced::Color {
    r: 0.0,
    g: 0.0,
    b: 0.8,
    a: 1.0,
};

/// Color for the snake heads (slightly different from body)
pub const SNAKE_HEAD_COLOR: iced::Color = iced::Color {
    r: 0.0,
    g: 0.0,
    b: 0.0,
    a: 1.0,
};

/// Color for food (red)
pub const FOOD_COLOR: iced::Color = iced::Color {
    r: 0.8,
    g: 0.0,
    b: 0.0,
    a: 1.0,
};

/// Color for grid background
pub const GRID_COLOR: iced::Color = iced::Color {
    r: 0.9,
    g: 0.9,
    b: 0.9,
    a: 1.0,
};

/// Color for grid lines
pub const GRID_LINE_COLOR: iced::Color = iced::Color {
    r: 0.8,
    g: 0.8,
    b: 0.8,
    a: 1.0,
};