// In src/logic/single_player.rs
use crate::logic::snake::{Snake, Direction};
use crate::logic::food::Food;
use crate::logic::grid::Grid;
use crate::logic::collision::CollisionDetector;
use crate::utils::constants::{INITIAL_SNAKE_LENGTH};

/// Main game logic for single player mode
pub struct SinglePlayerGame {
    /// The grid for the game
    pub grid: Grid,
    /// The player's snake
    pub snake: Snake,
    /// Current food item
    pub food: Food,
    /// Current score
    pub score: u32,
    /// High score (persists between games)
    pub high_score: u32,
    /// Is the game over
    pub game_over: bool,
}

impl SinglePlayerGame {
    /// Creates a new single player game
    pub fn new() -> Self {
        let grid = Grid::new();
        
        // Create snake in the middle of the grid
        let start_pos = grid.center();
        let snake = Snake::new(start_pos.x, start_pos.y, INITIAL_SNAKE_LENGTH);
        
        // Create initial food
        let food = Food::new_random(&snake.body);
        
        Self {
            grid,
            snake,
            food,
            score: 0,
            high_score: 0,
            game_over: false,
        }
    }
    
    /// Handles player input to change snake direction
    pub fn handle_input(&mut self, direction: Direction) {
        if !self.game_over {
            self.snake.change_direction(direction);
        }
    }
    
    /// Updates the game state for a single tick
    /// Returns true if the game is still running, false if game over
    pub fn update(&mut self) -> bool {
        if self.game_over {
            return false;
        }
        
        // Move the snake
        self.snake.move_forward();
        
        // Check collisions
        let (is_alive, ate_food) = CollisionDetector::check_single_player_collisions(
            &self.snake, 
            &self.food
        );
        
        if !is_alive {
            self.game_over = true;
            
            // Update high score if current score is higher
            if self.score > self.high_score {
                self.high_score = self.score;
            }
            
            return false;
        }
        
        // Handle food eating
        if ate_food {
            // Grow the snake
            self.snake.grow();
            
            // Increase score
            self.score += 1;
            
            // Generate new food
            self.food = Food::new_random(&self.snake.body);
        }
        
        true
    }
    
    /// Resets the game for a new round
    pub fn reset(&mut self) {
        let start_pos = self.grid.center();
        
        self.snake = Snake::new(start_pos.x, start_pos.y, INITIAL_SNAKE_LENGTH);
        self.food = Food::new_random(&self.snake.body);
        self.score = 0;
        self.game_over = false;
    }
    
    /// Gets the current score as a string
    pub fn score_text(&self) -> String {
        format!("Score: {}", self.score)
    }
    
    /// Gets the high score as a string
    pub fn high_score_text(&self) -> String {
        format!("High Score: {}", self.high_score)
    }
}