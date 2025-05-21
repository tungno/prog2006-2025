use serde::{Serialize, Deserialize};
use iced::Color;
use rand::Rng;
use crate::constants::{MUTATION_AMOUNT, MUTATION_RATE};

/// Represents the parameters for a heuristic strategy
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct StrategyParameters {
    pub name: String,
    pub food_weight: f32,
    pub water_weight: f32,
    pub energy_conservation: f32,
    pub exploration_tendency: f32,
    pub night_behavior: f32,
}

impl StrategyParameters {
    /// Create a new set of parameters with default values
    pub fn new(name: &str) -> Self {
        Self {
            name: name.to_string(),
            food_weight: 0.5,
            water_weight: 0.5,
            energy_conservation: 0.3,
            exploration_tendency: 0.2,
            night_behavior: 0.5,
        }
    }

    /// Create a new set of parameters with specific values
    pub fn with_values(
        name: &str,
        food_weight: f32,
        water_weight: f32,
        energy_conservation: f32,
        exploration_tendency: f32,
        night_behavior: f32,
    ) -> Self {
        Self {
            name: name.to_string(),
            food_weight,
            water_weight,
            energy_conservation,
            exploration_tendency,
            night_behavior,
        }
    }

    /// Clamp a value between min and max
    fn clamp(value: f32, min: f32, max: f32) -> f32 {
        if value < min {
            min
        } else if value > max {
            max
        } else {
            value
        }
    }

    /// Mutate parameters randomly based on mutation rate and amount
    pub fn mutate(&self, mutation_rate: Option<f32>, mutation_amount: Option<f32>) -> Self {
        let mut rng = rand::thread_rng();
        let rate = mutation_rate.unwrap_or(MUTATION_RATE);
        let amount = mutation_amount.unwrap_or(MUTATION_AMOUNT);
        
        let mut mutate_value = |value: f32| -> f32 {
            if rng.gen::<f32>() < rate {
                Self::clamp(value + (rng.gen::<f32>() * 2.0 - 1.0) * amount, 0.1, 0.9)
            } else {
                value
            }
        };
        
        Self {
            name: format!("Evolved_{}", self.name),
            food_weight: mutate_value(self.food_weight),
            water_weight: mutate_value(self.water_weight),
            energy_conservation: mutate_value(self.energy_conservation),
            exploration_tendency: mutate_value(self.exploration_tendency),
            night_behavior: mutate_value(self.night_behavior),
        }
    }

    /// Crossover this set of parameters with another set
    pub fn crossover(&self, other: &Self) -> Self {
        let mut rng = rand::thread_rng();
        
        // Perform crossover - for each parameter, randomly choose from either parent
        // or take the average with some random variation
        let mut crossover_value = |val1: f32, val2: f32| -> f32 {
            let choice = rng.gen_range(0..3);
            match choice {
                0 => val1,
                1 => val2,
                _ => {
                    // Average with small random variation
                    let avg = (val1 + val2) / 2.0;
                    Self::clamp(avg + (rng.gen::<f32>() * 0.2 - 0.1), 0.1, 0.9)
                }
            }
        };
        
        Self {
            name: "Crossover".to_string(),
            food_weight: crossover_value(self.food_weight, other.food_weight),
            water_weight: crossover_value(self.water_weight, other.water_weight),
            energy_conservation: crossover_value(self.energy_conservation, other.energy_conservation),
            exploration_tendency: crossover_value(self.exploration_tendency, other.exploration_tendency),
            night_behavior: crossover_value(self.night_behavior, other.night_behavior),
        }
    }

    /// Generate a color based on the strategy parameters
    pub fn generate_color(&self) -> Color {
        // Create a distinguishable color based on parameter values
        let h = (self.food_weight * 120.0 + self.water_weight * 240.0) % 360.0;
        let s = 0.8;
        let v = 0.9;
        
        // Convert HSV to RGB
        let c = v * s;
        let x = c * (1.0 - ((h / 60.0) % 2.0 - 1.0).abs());
        let m = v - c;
        
        let (r, g, b) = if h < 60.0 {
            (c, x, 0.0)
        } else if h < 120.0 {
            (x, c, 0.0)
        } else if h < 180.0 {
            (0.0, c, x)
        } else if h < 240.0 {
            (0.0, x, c)
        } else if h < 300.0 {
            (x, 0.0, c)
        } else {
            (c, 0.0, x)
        };
        
        Color::from_rgb(r + m, g + m, b + m)
    }
}

/// Predefined strategy templates
pub fn default_strategies() -> Vec<StrategyParameters> {
    vec![
        StrategyParameters::with_values(
            "Balanced",
            0.5, 0.5, 0.3, 0.2, 0.5,
        ),
        StrategyParameters::with_values(
            "FoodFocused",
            0.8, 0.2, 0.2, 0.1, 0.4,
        ),
        StrategyParameters::with_values(
            "WaterFocused",
            0.2, 0.8, 0.2, 0.1, 0.4,
        ),
        StrategyParameters::with_values(
            "Explorer",
            0.4, 0.4, 0.1, 0.7, 0.3,
        ),
        StrategyParameters::with_values(
            "Conservative",
            0.4, 0.4, 0.7, 0.1, 0.7,
        ),
    ]
}