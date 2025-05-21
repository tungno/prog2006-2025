use rand::Rng;
use std::collections::HashMap;
use crate::heuristics::parameters::StrategyParameters;
use crate::environment::{TimeOfDay, grid::Grid};
use crate::environment::cell::CellType;
use crate::constants::{SENSING_RADIUS, GRID_SIZE};

/// Represents a visible resource location with its distance
#[derive(Debug, Clone)]
pub struct VisibleItem {
    pub x: usize,
    pub y: usize,
    pub distance: usize,
}

/// Represents all resources visible to an agent
#[derive(Debug, Clone)]
pub struct VisibleResources {
    pub food: Vec<VisibleItem>,
    pub water: Vec<VisibleItem>,
    pub obstacles: Vec<VisibleItem>,
    pub agents: Vec<VisibleItem>,
}

/// Direction for agent movement
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Direction {
    North,
    East,
    South,
    West,
}

impl Direction {
    /// Get the x and y deltas for this direction
    pub fn deltas(&self) -> (isize, isize) {
        match self {
            Direction::North => (0, -1),
            Direction::East => (1, 0),
            Direction::South => (0, 1),
            Direction::West => (-1, 0),
        }
    }
    
    /// Get all possible directions
    pub fn all() -> Vec<Direction> {
        vec![
            Direction::North,
            Direction::East,
            Direction::South,
            Direction::West,
        ]
    }
    
    /// Get a random direction
    pub fn random() -> Direction {
        let mut rng = rand::thread_rng();
        let directions = Self::all();
        directions[rng.gen_range(0..directions.len())]
    }
}

/// The strategy trait defines how agents make decisions
pub trait Strategy: std::fmt::Debug {
    /// Get the name of this strategy
    fn name(&self) -> &str;
    
    /// Get the parameters for this strategy
    fn parameters(&self) -> &StrategyParameters;
    
    /// Clone this strategy
    fn clone_box(&self) -> Box<dyn Strategy>;
    
    /// Sense resources within the sensing radius
    fn sense(&self, grid: &Grid, x: usize, y: usize, radius: usize) -> VisibleResources {
        let mut resources = VisibleResources {
            food: Vec::new(),
            water: Vec::new(),
            obstacles: Vec::new(),
            agents: Vec::new(),
        };
        
        // Scan the area within the sensing radius
        for dy in -(radius as isize)..=(radius as isize) {
            for dx in -(radius as isize)..=(radius as isize) {
                let nx = x as isize + dx;
                let ny = y as isize + dy;
                
                // Skip if out of bounds
                if nx < 0 || nx >= GRID_SIZE as isize || ny < 0 || ny >= GRID_SIZE as isize {
                    continue;
                }
                
                let nx = nx as usize;
                let ny = ny as usize;
                
                // Skip self
                if nx == x && ny == y {
                    continue;
                }
                
                // Calculate Manhattan distance
                let distance = dx.abs() as usize + dy.abs() as usize;
                
                // Skip if outside the radius
                if distance > radius {
                    continue;
                }
                
                if let Some(cell) = grid.get_cell(nx, ny) {
                    match cell.cell_type {
                        CellType::Food => resources.food.push(VisibleItem { x: nx, y: ny, distance }),
                        CellType::Water => resources.water.push(VisibleItem { x: nx, y: ny, distance }),
                        CellType::Obstacle => resources.obstacles.push(VisibleItem { x: nx, y: ny, distance }),
                        CellType::Agent => resources.agents.push(VisibleItem { x: nx, y: ny, distance }),
                        _ => {}
                    }
                }
            }
        }
        
        resources
    }
    
    /// Decide on the next move based on current state and visible resources
    fn decide_move(
        &self,
        resources: &VisibleResources,
        hunger: f32,
        thirst: f32,
        energy: f32,
        time_of_day: TimeOfDay,
        agent_x: usize,
        agent_y: usize,
    ) -> Option<Direction>;
    
    /// Calculate the movement during night time (usually more conservative)
    fn night_time_move(
        &self,
        resources: &VisibleResources,
        hunger: f32,
        thirst: f32,
        energy: f32,
        agent_x: usize,
        agent_y: usize,
    ) -> Option<Direction>;
}

/// The standard strategy uses the parametrized approach for decision making
#[derive(Debug, Clone)]
pub struct StandardStrategy {
    parameters: StrategyParameters,
}

impl StandardStrategy {
    /// Create a new standard strategy with the given parameters
    pub fn new(parameters: StrategyParameters) -> Self {
        Self { parameters }
    }
}

impl Strategy for StandardStrategy {
    fn name(&self) -> &str {
        &self.parameters.name
    }
    
    fn parameters(&self) -> &StrategyParameters {
        &self.parameters
    }
    
    fn clone_box(&self) -> Box<dyn Strategy> {
        Box::new(self.clone())
    }
    
    fn decide_move(
        &self,
        resources: &VisibleResources,
        hunger: f32,
        thirst: f32,
        energy: f32,
        time_of_day: TimeOfDay,
        agent_x: usize,
        agent_y: usize,
    ) -> Option<Direction> {
        let mut rng = rand::thread_rng();
        
        // Handle night time behavior
        if time_of_day == TimeOfDay::Night && rng.gen::<f32>() < self.parameters.night_behavior {
            return self.night_time_move(resources, hunger, thirst, energy, agent_x, agent_y);
        }
        
        // Random exploration chance
        if rng.gen::<f32>() < self.parameters.exploration_tendency {
            return Some(Direction::random());
        }
        
        // Calculate urgency levels
        let hunger_urgency = (100.0 - hunger) / 100.0;
        let thirst_urgency = (100.0 - thirst) / 100.0;
        
        // Calculate priorities based on urgency and strategy weights
        let food_priority = hunger_urgency * self.parameters.food_weight;
        let water_priority = thirst_urgency * self.parameters.water_weight;
        
        // Choose target based on priority
        let target = if food_priority > water_priority && !resources.food.is_empty() {
            // Sort by distance and pick the closest
            let mut food = resources.food.clone();
            food.sort_by(|a, b| a.distance.cmp(&b.distance));
            Some(food[0].clone())
        } else if water_priority >= food_priority && !resources.water.is_empty() {
            // Sort by distance and pick the closest
            let mut water = resources.water.clone();
            water.sort_by(|a, b| a.distance.cmp(&b.distance));
            Some(water[0].clone())
        } else if !resources.food.is_empty() {
            // Default to food if available
            let mut food = resources.food.clone();
            food.sort_by(|a, b| a.distance.cmp(&b.distance));
            Some(food[0].clone())
        } else if !resources.water.is_empty() {
            // Default to water if available
            let mut water = resources.water.clone();
            water.sort_by(|a, b| a.distance.cmp(&b.distance));
            Some(water[0].clone())
        } else {
            None
        };
        
        if let Some(target) = target {
            // Move towards the target
            self.calculate_direction_to_target(target.x, target.y, agent_x, agent_y)
        } else {
            None
        }
    }
    
    fn night_time_move(
        &self,
        resources: &VisibleResources,
        hunger: f32,
        thirst: f32,
        energy: f32,
        agent_x: usize,
        agent_y: usize,
    ) -> Option<Direction> {
        let mut rng = rand::thread_rng();
        
        // If very low on a resource and it's visible, go for it
        if hunger < 30.0 && !resources.food.is_empty() {
            let mut food = resources.food.clone();
            food.sort_by(|a, b| a.distance.cmp(&b.distance));
            let target = &food[0];
            return self.calculate_direction_to_target(target.x, target.y, agent_x, agent_y);
        }
        
        if thirst < 30.0 && !resources.water.is_empty() {
            let mut water = resources.water.clone();
            water.sort_by(|a, b| a.distance.cmp(&b.distance));
            let target = &water[0];
            return self.calculate_direction_to_target(target.x, target.y, agent_x, agent_y);
        }
        
        // If energy is low, rest (do not move)
        if energy < 50.0 && rng.gen::<f32>() < self.parameters.energy_conservation {
            return None;
        }
        
        // If neither hungry nor thirsty, move very little or not at all
        if rng.gen::<f32>() < 0.7 {
            return None; // Stay in place
        }
        
        // Occasionally make a small random move
        Some(Direction::random())
    }
}

impl StandardStrategy {
    /// Calculate the direction to move towards a target
    fn calculate_direction_to_target(&self, target_x: usize, target_y: usize, agent_x: usize, agent_y: usize) -> Option<Direction> {
        // Helper function to find the best direction
        fn best_direction(dx: isize, dy: isize) -> Option<Direction> {
            // Prioritize the larger distance component
            if dx.abs() > dy.abs() {
                if dx > 0 {
                    Some(Direction::East)
                } else if dx < 0 {
                    Some(Direction::West)
                } else {
                    None
                }
            } else {
                if dy > 0 {
                    Some(Direction::South)
                } else if dy < 0 {
                    Some(Direction::North)
                } else {
                    None
                }
            }
        }
        
        best_direction(target_x as isize - agent_x as isize, 
                     target_y as isize - agent_y as isize)
    }
}