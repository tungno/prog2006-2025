use crate::logic::snake::{Snake, Position};
use crate::logic::food::Food;
use crate::utils::constants::GRID_SIZE;

/// Module for handling all collision detection in the game
pub struct CollisionDetector;

impl CollisionDetector {
    /// Checks if a position is outside the grid boundaries
    pub fn is_outside_grid(position: &Position) -> bool {
        position.x < 0 || position.x >= GRID_SIZE || 
        position.y < 0 || position.y >= GRID_SIZE
    }
    
    /// Checks if a snake has collided with itself
    pub fn check_self_collision(snake: &Snake) -> bool {
        let head = &snake.body[0];
        snake.body.iter().skip(1).any(|segment| segment == head)
    }
    
    /// Checks if a snake has collided with a wall
    pub fn check_wall_collision(snake: &Snake) -> bool {
        Self::is_outside_grid(&snake.head())
    }
    
    /// Checks if a snake has collided with another snake
    pub fn check_snake_collision(snake1: &Snake, snake2: &Snake) -> bool {
        let head1 = &snake1.body[0];
        
        // Check if snake1's head collides with any part of snake2
        snake2.body.iter().any(|segment| segment == head1)
    }
    
    /// Checks if a snake has eaten food
    pub fn check_food_collision(snake: &Snake, food: &Food) -> bool {
        let head = &snake.body[0];
        head.x == food.position.x && head.y == food.position.y
    }
    
    /// Checks all collisions for a single player game
    /// Returns true if the game should continue, false if game over
    pub fn check_single_player_collisions(snake: &Snake, food: &Food) -> (bool, bool) {
        // Check self collision
        if Self::check_self_collision(snake) {
            return (false, false); // Game over, no food eaten
        }
        
        // Check wall collision
        if Self::check_wall_collision(snake) {
            return (false, false); // Game over, no food eaten
        }
        
        // Check food collision
        let food_eaten = Self::check_food_collision(snake, food);
        
        (true, food_eaten) // Game continues, maybe ate food
    }
    
    /// Checks all collisions for a multiplayer game
    /// Returns (snake1_alive, snake2_alive, food_eaten_by)
    /// where food_eaten_by is 1 for snake1, 2 for snake2, or 0 for none
    pub fn check_multi_player_collisions(
        snake1: &Snake, 
        snake2: &Snake, 
        food: &Food
    ) -> (bool, bool, u8) {
        let mut snake1_alive = true;
        let mut snake2_alive = true;
        let mut food_eaten_by = 0;
        
        // Check snake1 collisions
        if Self::check_self_collision(snake1) || 
           Self::check_wall_collision(snake1) || 
           Self::check_snake_collision(snake1, snake2) {
            snake1_alive = false;
        }
        
        // Check snake2 collisions
        if Self::check_self_collision(snake2) || 
           Self::check_wall_collision(snake2) || 
           Self::check_snake_collision(snake2, snake1) {
            snake2_alive = false;
        }
        
        // Check food collisions
        if snake1_alive && Self::check_food_collision(snake1, food) {
            food_eaten_by = 1;
        } else if snake2_alive && Self::check_food_collision(snake2, food) {
            food_eaten_by = 2;
        }
        
        (snake1_alive, snake2_alive, food_eaten_by)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    
    #[test]
    fn test_outside_grid() {
        assert!(CollisionDetector::is_outside_grid(&Position::new(-1, 5)));
        assert!(CollisionDetector::is_outside_grid(&Position::new(5, -1)));
        assert!(CollisionDetector::is_outside_grid(&Position::new(GRID_SIZE, 5)));
        assert!(CollisionDetector::is_outside_grid(&Position::new(5, GRID_SIZE)));
        assert!(!CollisionDetector::is_outside_grid(&Position::new(0, 0)));
        assert!(!CollisionDetector::is_outside_grid(&Position::new(GRID_SIZE - 1, GRID_SIZE - 1)));
    }
}