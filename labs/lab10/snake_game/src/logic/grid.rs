use crate::logic::snake::Position;
use crate::utils::constants::GRID_SIZE;

/// Represents the game grid where the snake and food reside
pub struct Grid {
    /// Width of the grid in cells
    pub width: i32,
    /// Height of the grid in cells
    pub height: i32,
}

impl Grid {
    /// Creates a new grid with the default size
    pub fn new() -> Self {
        Self {
            width: GRID_SIZE,
            height: GRID_SIZE,
        }
    }
    
    /// Creates a new grid with custom dimensions
    pub fn with_size(width: i32, height: i32) -> Self {
        Self {
            width,
            height,
        }
    }
    
    /// Checks if a position is within the grid boundaries
    pub fn is_within_bounds(&self, position: &Position) -> bool {
        position.x >= 0 && position.x < self.width &&
        position.y >= 0 && position.y < self.height
    }
    
    /// Wraps a position around the grid boundaries if needed
    /// For example, if a snake moves off the right edge, it appears on the left
    pub fn wrap_position(&self, position: &Position) -> Position {
        Position {
            x: (position.x + self.width) % self.width,
            y: (position.y + self.height) % self.height,
        }
    }
    
    /// Gets all empty positions on the grid (not occupied by any snake)
    pub fn get_empty_positions(&self, occupied_positions: &[Position]) -> Vec<Position> {
        let mut empty_positions = Vec::new();
        
        for x in 0..self.width {
            for y in 0..self.height {
                let pos = Position::new(x, y);
                if !occupied_positions.contains(&pos) {
                    empty_positions.push(pos);
                }
            }
        }
        
        empty_positions
    }
    
    /// Gets a random empty position on the grid
    pub fn get_random_empty_position(&self, occupied_positions: &[Position]) -> Option<Position> {
        use rand::seq::SliceRandom;
        
        let empty_positions = self.get_empty_positions(occupied_positions);
        
        if empty_positions.is_empty() {
            None // Grid is full
        } else {
            // Select a random empty position
            let mut rng = rand::thread_rng();
            empty_positions.choose(&mut rng).copied()
        }
    }
    
    /// Checks if the grid is full (no empty positions)
    pub fn is_full(&self, occupied_positions: &[Position]) -> bool {
        (occupied_positions.len() as i32) >= self.width * self.height
    }
    
    /// Calculates the center position of the grid
    pub fn center(&self) -> Position {
        Position::new(self.width / 2, self.height / 2)
    }
}

impl Default for Grid {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    
    #[test]
    fn test_is_within_bounds() {
        let grid = Grid::new();
        
        assert!(grid.is_within_bounds(&Position::new(0, 0)));
        assert!(grid.is_within_bounds(&Position::new(GRID_SIZE - 1, GRID_SIZE - 1)));
        assert!(!grid.is_within_bounds(&Position::new(-1, 0)));
        assert!(!grid.is_within_bounds(&Position::new(0, -1)));
        assert!(!grid.is_within_bounds(&Position::new(GRID_SIZE, 0)));
        assert!(!grid.is_within_bounds(&Position::new(0, GRID_SIZE)));
    }
    
    #[test]
    fn test_wrap_position() {
        let grid = Grid::new();
        
        // Test wrapping right to left
        let pos = Position::new(GRID_SIZE, 5);
        let wrapped = grid.wrap_position(&pos);
        assert_eq!(wrapped.x, 0);
        assert_eq!(wrapped.y, 5);
        
        // Test wrapping bottom to top
        let pos = Position::new(5, GRID_SIZE);
        let wrapped = grid.wrap_position(&pos);
        assert_eq!(wrapped.x, 5);
        assert_eq!(wrapped.y, 0);
        
        // Test wrapping left to right
        let pos = Position::new(-1, 5);
        let wrapped = grid.wrap_position(&pos);
        assert_eq!(wrapped.x, GRID_SIZE - 1);
        assert_eq!(wrapped.y, 5);
        
        // Test wrapping top to bottom
        let pos = Position::new(5, -1);
        let wrapped = grid.wrap_position(&pos);
        assert_eq!(wrapped.x, 5);
        assert_eq!(wrapped.y, GRID_SIZE - 1);
    }
}