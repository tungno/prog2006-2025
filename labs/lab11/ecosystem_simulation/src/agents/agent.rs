use std::collections::VecDeque;
use uuid::Uuid;
use iced::Color;
use crate::constants::{SENSING_RADIUS, MAX_AGE};
use crate::environment::{TimeOfDay, grid::Grid, cell::CellType};
use crate::heuristics::{Strategy, StandardStrategy, StrategyParameters, Direction};

/// Represents data recorded in the agent's history
#[derive(Debug, Clone, PartialEq)]
pub struct HistoryPoint {
    pub health: f32,
    pub hunger: f32,
    pub thirst: f32,
    pub energy: f32,
}

/// Represents an agent's position
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Position {
    pub x: usize,
    pub y: usize,
}

/// Represents an autonomous agent in the simulation
#[derive(Debug)]
pub struct Agent {
    pub id: String,
    pub x: usize,
    pub y: usize,
    pub hunger: f32,
    pub thirst: f32,
    pub energy: f32,
    pub health: f32,
    pub age: u32,
    pub strategy_name: String,
    pub strategy: Box<dyn Strategy>,
    pub alive: bool,
    pub color: Color,
    pub history: VecDeque<HistoryPoint>,
    pub movement_history: VecDeque<Position>,
}

impl Agent {
    /// Create a new agent with the given strategy
    pub fn new(x: usize, y: usize, strategy_name: &str, strategy_params: StrategyParameters) -> Self {
        let id = Uuid::new_v4().to_string();
        let strategy = Box::new(StandardStrategy::new(strategy_params.clone()));
        let color = strategy_params.generate_color();
        
        Self {
            id,
            x,
            y,
            hunger: 50.0,
            thirst: 50.0,
            energy: 100.0,
            health: 100.0,
            age: 0,
            strategy_name: strategy_name.to_string(),
            strategy,
            alive: true,
            color,
            history: VecDeque::with_capacity(100),
            movement_history: VecDeque::with_capacity(20),
        }
    }

    /// Attempt to move the agent in the specified direction
    pub fn move_direction(&mut self, direction: Direction, grid: &mut Grid, time_of_day: TimeOfDay) -> bool {
        // Save last position for movement tracking
        self.movement_history.push_front(Position { x: self.x, y: self.y });
        if self.movement_history.len() > 20 {
            self.movement_history.pop_back();
        }
        
        let (dx, dy) = direction.deltas();
        let new_x = self.x as isize + dx;
        let new_y = self.y as isize + dy;
        
        // Check boundaries
        if !grid.is_within_bounds(new_x, new_y) {
            return false;
        }
        
        let new_x = new_x as usize;
        let new_y = new_y as usize;
        
        // Check for obstacles
        if let Some(cell) = grid.get_cell(new_x, new_y) {
            if cell.is_obstacle() {
                return false;
            }
            
            // Consume resource if present
            if cell.cell_type == CellType::Food {
                self.hunger = f32::min(100.0, self.hunger + 30.0);
            } else if cell.cell_type == CellType::Water {
                self.thirst = f32::min(100.0, self.thirst + 30.0);
            }
        }
        
        // Update agent's position in the grid
        if let Some(cell) = grid.get_cell_mut(self.x, self.y) {
            if cell.has_agent() && cell.agent_id.as_ref() == Some(&self.id) {
                // Clear the old position
                *cell = CellType::Empty.into();
            }
        }
        
        // Set the new position
        if let Some(cell) = grid.get_cell_mut(new_x, new_y) {
            *cell = CellType::Agent.into();
            cell.agent_id = Some(self.id.clone());
        }
        
        // Update agent's position
        self.x = new_x;
        self.y = new_y;
        
        // Consume energy for movement - more energy at night
        let energy_cost = if time_of_day == TimeOfDay::Night { 3.0 } else { 2.0 };
        self.energy -= energy_cost;
        
        true
    }

    /// Update the agent for one simulation step
    pub fn update(&mut self, grid: &mut Grid, time_of_day: TimeOfDay) {
        if !self.alive {
            return;
        }
        
        self.age += 1;
        
        // Update vitals - affected by time of day
        let hunger_rate = if time_of_day == TimeOfDay::Night { 0.8 } else { 1.0 };
        let thirst_rate = if time_of_day == TimeOfDay::Night { 1.8 } else { 1.5 };
        
        self.hunger = f32::max(0.0, self.hunger - hunger_rate);
        self.thirst = f32::max(0.0, self.thirst - thirst_rate);
        
        // Rest if low on energy and strategy suggests it
        if self.energy < 20.0 && rand::random::<f32>() < self.strategy.parameters().energy_conservation {
            self.energy = f32::min(100.0, self.energy + 10.0);
            self.record_history();
            return;
        }
        
        // Update health based on hunger and thirst
        self.health = 0.6 * (self.hunger + self.thirst) / 2.0;
        
        // Die if health reaches zero, or max age
        if self.hunger <= 0.0 || self.thirst <= 0.0 || self.energy <= 0.0 || self.age >= MAX_AGE {
            self.alive = false;
            return;
        }
        
        // Sense nearby resources within sensing radius
        let resources = self.strategy.sense(grid, self.x, self.y, SENSING_RADIUS);
        
        // Decide on next move based on strategy
        let direction = self.strategy.decide_move(
            &resources, 
            self.hunger, 
            self.thirst, 
            self.energy, 
            time_of_day,
            self.x,
            self.y,
        );
        
        if let Some(direction) = direction {
            // Try to move in the chosen direction
            self.move_direction(direction, grid, time_of_day);
        } else {
            // If no direction was chosen, and energy is low, rest
            if self.energy < 50.0 {
                self.energy = f32::min(100.0, self.energy + 5.0);
            }
        }
        
        // Record current stats in history
        self.record_history();
    }

    /// Record the current agent stats in its history
    fn record_history(&mut self) {
        let point = HistoryPoint {
            health: self.health,
            hunger: self.hunger,
            thirst: self.thirst,
            energy: self.energy,
        };
        
        self.history.push_front(point);
        
        // Limit history size
        if self.history.len() > 100 {
            self.history.pop_back();
        }
    }

    /// Calculate the fitness score used for evolution
    pub fn get_fitness_score(&self) -> f32 {
        self.age as f32 * 0.6 + self.health * 0.4
    }
    
    /// Check if the agent has been at position (x, y) in its recent movement history
    pub fn has_visited(&self, x: usize, y: usize) -> bool {
        self.movement_history.iter().any(|pos| pos.x == x && pos.y == y)
    }
}

impl Clone for Agent {
    fn clone(&self) -> Self {
        Self {
            id: self.id.clone(),
            x: self.x,
            y: self.y,
            hunger: self.hunger,
            thirst: self.thirst,
            energy: self.energy,
            health: self.health,
            age: self.age,
            strategy_name: self.strategy_name.clone(),
            strategy: self.strategy.clone_box(),
            alive: self.alive,
            color: self.color,
            history: self.history.clone(),
            movement_history: self.movement_history.clone(),
        }
    }
}

impl PartialEq for Agent {
    fn eq(&self, other: &Self) -> bool {
        // Compare agents by their ID
        self.id == other.id
    }
}