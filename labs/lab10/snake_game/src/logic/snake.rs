use crate::logic::food::Food;
use crate::utils::constants::GRID_SIZE;

/// Represents a direction the snake can move in
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Direction {
    /// Moving up (negative y)
    Up,
    /// Moving down (positive y)
    Down,
    /// Moving left (negative x)
    Left,
    /// Moving right (positive x)
    Right,
}

impl Direction {
    /// Returns true if the new direction is not opposite to the current direction
    pub fn is_valid_change(&self, new_direction: Direction) -> bool {
        match (*self, new_direction) {
            (Direction::Up, Direction::Down) => false,
            (Direction::Down, Direction::Up) => false,
            (Direction::Left, Direction::Right) => false,
            (Direction::Right, Direction::Left) => false,
            _ => true,
        }
    }
    
    /// Converts the direction to a (dx, dy) pair
    pub fn to_delta(&self) -> (i32, i32) {
        match *self {
            Direction::Up => (0, -1),
            Direction::Down => (0, 1),
            Direction::Left => (-1, 0),
            Direction::Right => (1, 0),
        }
    }
}

/// Represents a position on the game grid
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Position {
    /// X coordinate (horizontal)
    pub x: i32,
    /// Y coordinate (vertical)
    pub y: i32,
}

impl Position {
    /// Creates a new position
    pub fn new(x: i32, y: i32) -> Self {
        Self { x, y }
    }
    
    /// Moves the position in the given direction
    pub fn move_in_direction(&self, direction: Direction) -> Self {
        let (dx, dy) = direction.to_delta();
        Self {
            x: self.x + dx,
            y: self.y + dy,
        }
    }
    
    /// Wraps the position around the grid boundaries
    pub fn wrap(&self, grid_size: i32) -> Self {
        Self {
            x: (self.x + grid_size) % grid_size,
            y: (self.y + grid_size) % grid_size,
        }
    }
    
    /// Checks if the position is outside the grid boundaries
    pub fn is_outside_grid(&self, grid_size: i32) -> bool {
        self.x < 0 || self.x >= grid_size || self.y < 0 || self.y >= grid_size
    }
    
    /// Checks if this position equals a food position
    pub fn equals_food(&self, food: &Food) -> bool {
        self.x == food.position.x && self.y == food.position.y
    }
}

/// Represents a snake in the game
pub struct Snake {
    /// List of positions that make up the snake's body (head is at index 0)
    pub body: Vec<Position>,
    /// Direction the snake is currently moving
    pub direction: Direction,
    /// Flag indicating the snake should grow on the next move
    pub should_grow: bool,
}

impl Snake {
    /// Creates a new snake with the given starting position and length
    pub fn new(start_x: i32, start_y: i32, length: usize) -> Self {
        let mut body = Vec::with_capacity(length);
        
        // Create snake body vertically (tail below head)
        for i in 0..length {
            body.push(Position::new(start_x, start_y + i as i32));
        }
        
        Self {
            body,
            direction: Direction::Up,
            should_grow: false,
        }
    }
    
    /// Changes the snake's direction if valid
    pub fn change_direction(&mut self, new_direction: Direction) {
        if self.direction.is_valid_change(new_direction) {
            self.direction = new_direction;
        }
    }
    
    /// Moves the snake forward in its current direction
    pub fn move_forward(&mut self) {
        if self.body.is_empty() {
            return;
        }
        
        // Calculate new head position
        let head = self.body[0];
        let new_head_pos = head.move_in_direction(self.direction);
        
        // Wrap the position around the grid
        let new_head = Position {
            x: (new_head_pos.x + GRID_SIZE) % GRID_SIZE,
            y: (new_head_pos.y + GRID_SIZE) % GRID_SIZE,
        };
        
        // If snake should grow, don't remove the tail
        if self.should_grow {
            self.should_grow = false;
        } else {
            self.body.pop();
        }
        
        // Add new head
        self.body.insert(0, new_head);
    }
    
    /// Marks the snake to grow on the next movement
    pub fn grow(&mut self) {
        self.should_grow = true;
    }
    
    /// Checks if the snake's head has collided with its own body
    pub fn has_self_collision(&self) -> bool {
        if self.body.len() <= 1 {
            return false;
        }
        
        let head = self.body[0];
        for i in 1..self.body.len() {
            if self.body[i] == head {
                return true;
            }
        }
        
        false
    }
    
    /// Checks if the snake has collided with a wall
    pub fn has_wall_collision(&self, grid_size: i32) -> bool {
        let head = self.body[0];
        head.is_outside_grid(grid_size)
    }
    
    /// Checks if the snake has eaten food
    pub fn has_eaten(&self, food: &Food) -> bool {
        if self.body.is_empty() {
            return false;
        }
        
        self.body[0].equals_food(food)
    }
    
    /// Checks if the snake has collided with another snake
    pub fn has_collided_with_snake(&self, other: &Snake) -> bool {
        if self.body.is_empty() || other.body.is_empty() {
            return false;
        }
        
        let head = self.body[0];
        for segment in &other.body {
            if *segment == head {
                return true;
            }
        }
        
        false
    }
    
    /// Gets the snake's head position
    pub fn head(&self) -> Position {
        if self.body.is_empty() {
            // Return a default position if body is empty (should never happen)
            return Position::new(0, 0);
        }
        
        self.body[0]
    }
}