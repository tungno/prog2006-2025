// In src/state/multi_player_state.rs
use crate::logic::snake::{Snake, Direction};
use crate::logic::food::Food;
use crate::utils::constants::{GRID_SIZE, INITIAL_SNAKE_LENGTH};
use crate::utils::audio::{SoundEffect, play_sound};

/// Represents the state for a multiplayer game
pub struct MultiPlayerState {
    /// The first player's snake
    pub snake1: Snake,
    /// The second player's snake
    pub snake2: Snake,
    /// The food currently on the grid
    pub food: Food,
    /// First player's score
    pub score1: u32,
    /// Second player's score
    pub score2: u32,
    /// Whether the first player is still alive
    pub alive1: bool,
    /// Whether the second player is still alive
    pub alive2: bool,
    /// The winner of the game (1, 2, or None if tie or game still ongoing)
    pub winner: Option<u8>,
    /// Is the game over
    pub game_over: bool,
}

impl MultiPlayerState {
    /// Creates a new multiplayer state with default values
    pub fn new() -> Self {
        // Create snakes at different positions
        let mut snake1 = Snake::new(GRID_SIZE / 3, GRID_SIZE / 2, INITIAL_SNAKE_LENGTH);
        let mut snake2 = Snake::new(2 * GRID_SIZE / 3, GRID_SIZE / 2, INITIAL_SNAKE_LENGTH);
        
        // Set initial directions
        snake1.direction = Direction::Up;
        snake2.direction = Direction::Up;
        
        // Create food avoiding both snakes
        let mut occupied_positions = snake1.body.clone();
        occupied_positions.extend(snake2.body.clone());
        let food = Food::new_random(&occupied_positions);
        
        println!("Starting new multiplayer game");
        println!("Snake 1 starting position: ({}, {})", snake1.body[0].x, snake1.body[0].y);
        println!("Snake 2 starting position: ({}, {})", snake2.body[0].x, snake2.body[0].y);
        println!("Initial food position: ({}, {})", food.position.x, food.position.y);
        
        Self {
            snake1,
            snake2,
            food,
            score1: 0,
            score2: 0,
            alive1: true,
            alive2: true,
            winner: None,
            game_over: false,
        }
    }
    
    /// Updates the game state for a single tick
    /// Returns true if the game is still running, false if game over
    pub fn update(&mut self) -> bool {
        // Check if the game is already over
        if self.game_over {
            return false;
        }
        
        // Move the snakes if alive
        if self.alive1 {
            self.snake1.move_forward();
        }
        
        if self.alive2 {
            self.snake2.move_forward();
        }
        
        // Check collisions for Player 1
        if self.alive1 {
            // Get head position
            let head1 = self.snake1.head();
            
            // Check self collision
            let mut self_collision = false;
            for i in 1..self.snake1.body.len() {
                if head1 == self.snake1.body[i] {
                    self_collision = true;
                    break;
                }
            }
            
            if self_collision {
                println!("Player 1 collided with self");
                self.alive1 = false;
                
                // Mark Player 2 as winner and end game if Player 2 is alive
                if self.alive2 {
                    self.winner = Some(2);
                    self.game_over = true;
                    return false;
                }
            }
            
            // Check collision with snake 2
            if self.alive2 {
                for segment in &self.snake2.body {
                    if head1 == *segment {
                        println!("Player 1 collided with Player 2");
                        self.alive1 = false;
                        
                        // Mark Player 2 as winner and end game
                        self.winner = Some(2);
                        self.game_over = true;
                        return false;
                    }
                }
            }
            
            // Check food collision
            if self.alive1 && head1.equals_food(&self.food) {
                println!("Player 1 ate food at ({}, {})", self.food.position.x, self.food.position.y);
                self.snake1.grow();
                self.score1 += 1;
                println!("Player 1 score: {}", self.score1);
                
                // Play eat sound
                println!("PLAYING EAT SOUND FROM MULTI_PLAYER_STATE - PLAYER 1");
                play_sound(SoundEffect::Eat);
                
                self.generate_new_food();
            }
        }
        
        // Check collisions for Player 2
        if self.alive2 {
            // Get head position
            let head2 = self.snake2.head();
            
            // Check self collision
            let mut self_collision = false;
            for i in 1..self.snake2.body.len() {
                if head2 == self.snake2.body[i] {
                    self_collision = true;
                    break;
                }
            }
            
            if self_collision {
                println!("Player 2 collided with self");
                self.alive2 = false;
                
                // Mark Player 1 as winner and end game if Player 1 is alive
                if self.alive1 {
                    self.winner = Some(1);
                    self.game_over = true;
                    return false;
                }
            }
            
            // Check collision with snake 1
            if self.alive1 {
                for segment in &self.snake1.body {
                    if head2 == *segment {
                        println!("Player 2 collided with Player 1");
                        self.alive2 = false;
                        
                        // Mark Player 1 as winner and end game
                        self.winner = Some(1);
                        self.game_over = true;
                        return false;
                    }
                }
            }
            
            // Check food collision
            if self.alive2 && head2.equals_food(&self.food) {
                println!("Player 2 ate food at ({}, {})", self.food.position.x, self.food.position.y);
                self.snake2.grow();
                self.score2 += 1;
                println!("Player 2 score: {}", self.score2);
                
                // Play eat sound
                println!("PLAYING EAT SOUND FROM MULTI_PLAYER_STATE - PLAYER 2");
                play_sound(SoundEffect::Eat);
                
                self.generate_new_food();
            }
        }
        
        // If both players are now dead, determine the winner and end the game
        if !self.alive1 && !self.alive2 {
            self.determine_winner();
            self.game_over = true;
            return false;
        }
        
        // If only one player is now dead, the other is the winner
        if !self.alive1 && self.alive2 {
            self.winner = Some(2);
            println!("Player 1 died, Player 2 wins!");
            self.game_over = true;
            return false;
        } else if self.alive1 && !self.alive2 {
            self.winner = Some(1);
            println!("Player 2 died, Player 1 wins!");
            self.game_over = true;
            return false;
        }
        
        // Game continues
        true
    }
    
    /// Generates a new food item at a random position
    pub fn generate_new_food(&mut self) {
        let mut occupied_positions = Vec::new();
        
        // Add all snake segments to occupied positions
        occupied_positions.extend(self.snake1.body.clone());
        occupied_positions.extend(self.snake2.body.clone());
        
        self.food = Food::new_random(&occupied_positions);
        println!("Generated new food at position: ({}, {})", 
               self.food.position.x, self.food.position.y);
    }
    
    /// Determines the winner based on the current game state
    fn determine_winner(&mut self) {
        if self.alive1 && !self.alive2 {
            self.winner = Some(1);
            println!("Player 1 wins!");
        } else if !self.alive1 && self.alive2 {
            self.winner = Some(2);
            println!("Player 2 wins!");
        } else if self.score1 > self.score2 {
            self.winner = Some(1);
            println!("Player 1 wins with higher score: {} vs {}", self.score1, self.score2);
        } else if self.score2 > self.score1 {
            self.winner = Some(2);
            println!("Player 2 wins with higher score: {} vs {}", self.score2, self.score1);
        } else {
            // It's a tie
            self.winner = None;
            println!("It's a tie! Both players scored {}", self.score1);
        }
    }
    
    /// Resets the game state for a new game
    pub fn reset(&mut self) {
        // Create snakes at different positions
        let mut snake1 = Snake::new(GRID_SIZE / 3, GRID_SIZE / 2, INITIAL_SNAKE_LENGTH);
        let mut snake2 = Snake::new(2 * GRID_SIZE / 3, GRID_SIZE / 2, INITIAL_SNAKE_LENGTH);
        
        // Set initial directions
        snake1.direction = Direction::Up;
        snake2.direction = Direction::Up;
        
        self.snake1 = snake1;
        self.snake2 = snake2;
        
        // Generate new food
        let mut occupied_positions = self.snake1.body.clone();
        occupied_positions.extend(self.snake2.body.clone());
        self.food = Food::new_random(&occupied_positions);
        
        println!("Game reset");
        println!("Snake 1 position: ({}, {})", self.snake1.body[0].x, self.snake1.body[0].y);
        println!("Snake 2 position: ({}, {})", self.snake2.body[0].x, self.snake2.body[0].y);
        println!("Food position: ({}, {})", self.food.position.x, self.food.position.y);
        
        // Reset game state
        self.score1 = 0;
        self.score2 = 0;
        self.alive1 = true;
        self.alive2 = true;
        self.winner = None;
        self.game_over = false;
    }
}