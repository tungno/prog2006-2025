pub mod cell;
pub mod grid;
pub mod resources;

use cell::CellType;
use grid::Grid;
use resources::ResourceManager;
use crate::constants::{INITIAL_FOOD, INITIAL_WATER, INITIAL_OBSTACLES};

/// Represents the time of day in the simulation
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TimeOfDay {
    Day,
    Night,
}

impl TimeOfDay {
    pub fn get_day_factor(&self) -> f32 {
        match self {
            TimeOfDay::Day => 1.0,
            TimeOfDay::Night => 0.5, // Reduced regeneration at night
        }
    }
}

/// The main environment containing the grid and resources
pub struct Environment {
    pub grid: Grid,
    pub resource_manager: ResourceManager,
    pub time_of_day: TimeOfDay,
    pub day_counter: u32,
}

impl Environment {
    /// Create a new environment with initial setup
    pub fn new() -> Self {
        let mut grid = Grid::new();
        
        // Place initial entities
        grid.place_entities_randomly(CellType::Obstacle, INITIAL_OBSTACLES);
        grid.place_entities_randomly(CellType::Food, INITIAL_FOOD);
        grid.place_entities_randomly(CellType::Water, INITIAL_WATER);
        
        let resource_manager = ResourceManager::new();
        
        Self {
            grid,
            resource_manager,
            time_of_day: TimeOfDay::Day,
            day_counter: 0,
        }
    }

    /// Get the number of food cells in the grid
    pub fn food_count(&self) -> usize {
        self.grid.count_cells_of_type(CellType::Food)
    }

    /// Get the number of water cells in the grid
    pub fn water_count(&self) -> usize {
        self.grid.count_cells_of_type(CellType::Water)
    }

    /// Update the time of day based on the day counter
    pub fn update_time_of_day(&mut self, day_counter: u32, day_length: u32) {
        let half_day = day_length / 2;
        
        if day_counter == 0 {
            self.time_of_day = TimeOfDay::Day;
        } else if day_counter == half_day {
            self.time_of_day = TimeOfDay::Night;
        }
        
        self.day_counter = day_counter;
    }

    /// Regenerate resources based on current time of day
    pub fn regenerate_resources(&mut self) {
        let day_factor = self.time_of_day.get_day_factor();
        self.resource_manager.regenerate_resources(&mut self.grid, day_factor);
    }
}