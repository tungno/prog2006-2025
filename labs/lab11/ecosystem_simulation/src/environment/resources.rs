use crate::constants::{FOOD_REGENERATION_RATE, WATER_REGENERATION_RATE};
use crate::environment::cell::CellType;
use crate::environment::grid::Grid;
use rand::Rng;

/// Manages resource (food and water) regeneration in the grid
pub struct ResourceManager {
    food_regen_rate: f32,
    water_regen_rate: f32,
}

impl ResourceManager {
    /// Create a new resource manager with default regeneration rates
    pub fn new() -> Self {
        Self {
            food_regen_rate: FOOD_REGENERATION_RATE,
            water_regen_rate: WATER_REGENERATION_RATE,
        }
    }

    /// Create a new resource manager with custom regeneration rates
    pub fn with_rates(food_regen_rate: f32, water_regen_rate: f32) -> Self {
        Self {
            food_regen_rate,
            water_regen_rate,
        }
    }

    /// Update resource regeneration rates
    pub fn update_rates(&mut self, food_regen_rate: f32, water_regen_rate: f32) {
        self.food_regen_rate = food_regen_rate;
        self.water_regen_rate = water_regen_rate;
    }

    /// Regenerate resources based on current rates and day/night factor
    pub fn regenerate_resources(&self, grid: &mut Grid, day_factor: f32) {
        let mut rng = rand::thread_rng();
        
        // Regenerate food
        if rng.gen::<f32>() < self.food_regen_rate * day_factor {
            if let Some((x, y)) = grid.find_random_empty_cell() {
                grid.set_cell(x, y, CellType::Food);
            }
        }
        
        // Regenerate water
        if rng.gen::<f32>() < self.water_regen_rate * day_factor {
            if let Some((x, y)) = grid.find_random_empty_cell() {
                grid.set_cell(x, y, CellType::Water);
            }
        }
    }

    /// Get the current food regeneration rate
    pub fn food_regeneration_rate(&self) -> f32 {
        self.food_regen_rate
    }

    /// Get the current water regeneration rate
    pub fn water_regeneration_rate(&self) -> f32 {
        self.water_regen_rate
    }
}