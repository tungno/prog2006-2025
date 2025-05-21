// In src/state/single_player_state.rs
use crate::logic::snake::Snake;
use crate::logic::food::Food;
use crate::utils::constants::{GRID_SIZE, INITIAL_SNAKE_LENGTH};
use crate::utils::audio::{SoundEffect, play_sound};

/// Represents the state for a single player game
pub struct SinglePlayerState {
    /// The player's snake
    pub snake: Snake,
    /// The food currently on the grid
    pub food: Food,
    /// Current score (increases with each food eaten)
    pub score: u32,
    /// High score (persists between games)
    pub high_score: u32,
    /// Whether the game is over
    pub game_over: bool,
}

impl SinglePlayerState {
    /// Creates a new single player state with default values
    pub fn new() -> Self {
        // Create snake in the middle of the grid
        let start_x = GRID_SIZE / 2;
        let start_y = GRID_SIZE / 2;
        
        let snake = Snake::new(start_x, start_y, INITIAL_SNAKE_LENGTH);
        
        // Create initial food at a random position
        let food = Food::new_random(&snake.body);
        
        Self {
            snake,
            food,
            score: 0,
            high_score: 0,
            game_over: false,
        }
    }
    
    /// Generate new food at a position not occupied by the snake
    pub fn generate_new_food(&mut self) {
        self.food = Food::new_random(&self.snake.body);
        println!("Generated new food at position: ({}, {})", 
               self.food.position.x, self.food.position.y);
    }
    
    /// Updates the game state for a single tick
    /// Returns true if the game is still running, false if game over
    pub fn update(&mut self) -> bool {
        // First check if the game is already over
        if self.game_over {
            return false;
        }
        
        // Move the snake
        self.snake.move_forward();
        
        // Get the head position for collision checking
        let head = self.snake.head();
        
        // Check for collisions with self
        for i in 1..self.snake.body.len() {
            if head == self.snake.body[i] {
                println!("Game over: Snake collided with itself");
                
                // Update high score if needed
                if self.score > self.high_score {
                    self.high_score = self.score;
                    println!("New high score: {}", self.high_score);
                }
                
                // Mark game as over and stop updating
                self.game_over = true;
                return false;
            }
        }
        
        // Check for wall collision if we want to implement that
        // This is commented out since the game uses wrap-around instead
        /*
        if head.x < 0 || head.x >= GRID_SIZE || head.y < 0 || head.y >= GRID_SIZE {
            println!("Game over: Snake collided with wall");
            
            // Update high score if needed
            if self.score > self.high_score {
                self.high_score = self.score;
            }
            
            // Mark game as over and stop updating
            self.game_over = true;
            return false;
        }
        */
        
        // Check for food collision
        if head.equals_food(&self.food) {
            println!("Yum! Snake ate food at position: ({}, {})", 
                   self.food.position.x, self.food.position.y);
            // Grow the snake
            self.snake.grow();
            
            // Increase score
            self.score += 1;
            println!("Score increased to: {}", self.score);
            
            // Play the eat sound
            println!("PLAYING EAT SOUND FROM SINGLE_PLAYER_STATE");
            play_sound(SoundEffect::Eat);
            
            // Update high score if needed
            if self.score > self.high_score {
                self.high_score = self.score;
                println!("New high score: {}", self.high_score);
            }
            
            // Generate new food
            self.generate_new_food();
        }
        
        // Game continues
        true
    }
    
    /// Resets the game state for a new game
    pub fn reset(&mut self) {
        let start_x = GRID_SIZE / 2;
        let start_y = GRID_SIZE / 2;
        
        self.snake = Snake::new(start_x, start_y, INITIAL_SNAKE_LENGTH);
        self.generate_new_food();
        self.score = 0;
        self.game_over = false;
        // We keep the high score
    }
}