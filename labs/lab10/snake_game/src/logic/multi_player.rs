use crate::logic::snake::{Snake, Direction};
use crate::logic::food::Food;
use crate::logic::grid::Grid;
use crate::logic::collision::CollisionDetector;
use crate::utils::constants::{GRID_SIZE, INITIAL_SNAKE_LENGTH};

/// Main game logic for multi-player mode
pub struct MultiPlayerGame {
    /// The grid for the game
    pub grid: Grid,
    /// The first player's snake
    pub snake1: Snake,
    /// The second player's snake
    pub snake2: Snake,
    /// Current food item
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

impl MultiPlayerGame {
    /// Creates a new multi-player game
    pub fn new() -> Self {
        let grid = Grid::new();
        
        // Create snakes at different positions of the grid
        let mut snake1 = Snake::new(GRID_SIZE / 3, GRID_SIZE / 2, INITIAL_SNAKE_LENGTH);
        let mut snake2 = Snake::new(2 * GRID_SIZE / 3, GRID_SIZE / 2, INITIAL_SNAKE_LENGTH);
        
        // Initial snake directions - facing opposite directions
        snake1.direction = Direction::Up;
        snake2.direction = Direction::Up;
        
        // Create food avoiding both snakes
        let mut occupied_positions = snake1.body.clone();
        occupied_positions.extend(snake2.body.clone());
        let food = Food::new_random(&occupied_positions);
        
        Self {
            grid,
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
    
    /// Handles input for player 1
    pub fn handle_input_p1(&mut self, direction: Direction) {
        if self.alive1 && !self.game_over {
            self.snake1.change_direction(direction);
        }
    }
    
    /// Handles input for player 2
    pub fn handle_input_p2(&mut self, direction: Direction) {
        if self.alive2 && !self.game_over {
            self.snake2.change_direction(direction);
        }
    }
    
    /// Updates the game state for a single tick
    /// Returns true if the game is still running, false if game over
    pub fn update(&mut self) -> bool {
        if self.game_over {
            return false;
        }
        
        // Move the snakes if they're alive
        if self.alive1 {
            self.snake1.move_forward();
        }
        
        if self.alive2 {
            self.snake2.move_forward();
        }
        
        // Check collisions
        let (snake1_alive, snake2_alive, food_eaten_by) = CollisionDetector::check_multi_player_collisions(
            &self.snake1,
            &self.snake2,
            &self.food
        );
        
        // Update alive status
        let was_alive1 = self.alive1;
        let was_alive2 = self.alive2;
        
        self.alive1 = snake1_alive;
        self.alive2 = snake2_alive;
        
        // Handle food eating
        match food_eaten_by {
            1 => {
                self.snake1.grow();
                self.score1 += 1;
                self.generate_new_food();
            },
            2 => {
                self.snake2.grow();
                self.score2 += 1;
                self.generate_new_food();
            },
            _ => {} // No one ate food
        }
        
        // Check if the game is over
        if !self.alive1 && !self.alive2 {
            self.game_over = true;
            self.determine_winner();
            return false;
        } else if was_alive1 && !self.alive1 && was_alive2 && self.alive2 {
            // Player 1 just died, Player 2 is still alive
            self.winner = Some(2);
        } else if was_alive2 && !self.alive2 && was_alive1 && self.alive1 {
            // Player 2 just died, Player 1 is still alive
            self.winner = Some(1);
        }
        
        true
    }
    
    /// Generates a new food item at a random position
    fn generate_new_food(&mut self) {
        let mut occupied_positions = Vec::new();
        
        // Add all snake segments to occupied positions
        if self.alive1 {
            occupied_positions.extend(self.snake1.body.clone());
        }
        
        if self.alive2 {
            occupied_positions.extend(self.snake2.body.clone());
        }
        
        self.food = Food::new_random(&occupied_positions);
    }
    
    /// Determines the winner based on the current game state
    fn determine_winner(&mut self) {
        if self.alive1 && !self.alive2 {
            self.winner = Some(1);
        } else if !self.alive1 && self.alive2 {
            self.winner = Some(2);
        } else if self.score1 > self.score2 {
            self.winner = Some(1);
        } else if self.score2 > self.score1 {
            self.winner = Some(2);
        } else {
            // It's a tie
            self.winner = None;
        }
    }
    
    /// Resets the game for a new round
    pub fn reset(&mut self) {
        // Create snakes at different positions
        self.snake1 = Snake::new(GRID_SIZE / 3, GRID_SIZE / 2, INITIAL_SNAKE_LENGTH);
        self.snake2 = Snake::new(2 * GRID_SIZE / 3, GRID_SIZE / 2, INITIAL_SNAKE_LENGTH);
        
        // Reset directions
        self.snake1.direction = Direction::Up;
        self.snake2.direction = Direction::Up;
        
        // Generate new food
        let mut occupied_positions = self.snake1.body.clone();
        occupied_positions.extend(self.snake2.body.clone());
        self.food = Food::new_random(&occupied_positions);
        
        // Reset game state
        self.score1 = 0;
        self.score2 = 0;
        self.alive1 = true;
        self.alive2 = true;
        self.winner = None;
        self.game_over = false;
    }
    
    /// Gets the score text for player 1
    pub fn score1_text(&self) -> String {
        format!("Player 1: {}", self.score1)
    }
    
    /// Gets the score text for player 2
    pub fn score2_text(&self) -> String {
        format!("Player 2: {}", self.score2)
    }
    
    /// Gets the winner text
    pub fn winner_text(&self) -> String {
        match self.winner {
            Some(1) => "Player 1 Wins!".to_string(),
            Some(2) => "Player 2 Wins!".to_string(),
            None => if self.game_over { "It's a Tie!".to_string() } else { "Game in progress".to_string() },
            Some(_) => "Invalid winner".to_string(), // Catch any other values
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    
    #[test]
    fn test_new_game() {
        let game = MultiPlayerGame::new();
        assert_eq!(game.score1, 0);
        assert_eq!(game.score2, 0);
        assert!(game.alive1);
        assert!(game.alive2);
        assert_eq!(game.winner, None);
        assert!(!game.game_over);
    }
    
    #[test]
    fn test_handle_input() {
        let mut game = MultiPlayerGame::new();
        
        // Test player 1 input
        game.handle_input_p1(Direction::Left);
        assert_eq!(game.snake1.direction, Direction::Left);
        
        // Test player 2 input
        game.handle_input_p2(Direction::Right);
        assert_eq!(game.snake2.direction, Direction::Right);
    }
}