use rand::Rng;
use crate::logic::snake::Position;
use crate::utils::constants::GRID_SIZE;

/// Represents a food item in the game
pub struct Food {
    /// Position of the food on the grid
    pub position: Position,
}

impl Food {
    /// Creates a new food at a random position
    pub fn new_random(occupied_positions: &[Position]) -> Self {
        let mut rng = rand::thread_rng();
        let mut position;
        
        // Keep generating positions until we find one that's not occupied
        let mut attempts = 0;
        
        loop {
            let x = rng.gen_range(0..GRID_SIZE);
            let y = rng.gen_range(0..GRID_SIZE);
            position = Position::new(x, y);
            
            println!("Trying food position: ({}, {})", x, y);
            
            // Check if position is occupied by any part of the snake(s)
            let mut is_occupied = false;
            for pos in occupied_positions {
                if *pos == position {
                    is_occupied = true;
                    break;
                }
            }
            
            if !is_occupied {
                println!("Found valid food position: ({}, {})", x, y);
                break;
            }
            
            attempts += 1;
            
            // Emergency exit after too many attempts - prevents infinite loop
            // if the grid is too full (which shouldn't happen in normal gameplay)
            if attempts > 100 {
                println!("WARNING: Couldn't find empty position after 100 attempts, using last tried position");
                break;
            }
        }
        
        Self { position }
    }
    
    /// Creates a new food at a specific position (for testing)
    #[cfg(test)]
    pub fn new(x: i32, y: i32) -> Self {
        Self {
            position: Position::new(x, y),
        }
    }
    
    /// Creates a food at the center of the grid (fallback for emergencies)
    pub fn new_fallback() -> Self {
        println!("Creating fallback food at center of grid");
        Self {
            position: Position::new(GRID_SIZE / 2, GRID_SIZE / 2),
        }
    }
}