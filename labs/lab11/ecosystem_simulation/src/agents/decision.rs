use crate::agents::agent::Agent;
use crate::environment::{TimeOfDay, grid::Grid};
use crate::heuristics::{Direction, VisibleResources};

/// Agent decision types for statistics
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum DecisionType {
    SeekFood,
    SeekWater,
    Rest,
    Explore,
    Conservative,
}

/// Decision-making helper utilities for agents
pub struct DecisionMaker;

impl DecisionMaker {
    /// Calculate the hunger and thirst urgency based on agent stats
    pub fn calculate_urgency(agent: &Agent) -> (f32, f32) {
        let hunger_urgency = (100.0 - agent.hunger) / 100.0;
        let thirst_urgency = (100.0 - agent.thirst) / 100.0;
        
        (hunger_urgency, thirst_urgency)
    }
    
    /// Determine what resource to prioritize based on agent needs
    pub fn determine_priority(agent: &Agent) -> DecisionType {
        let (hunger_urgency, thirst_urgency) = Self::calculate_urgency(agent);
        let strategy = agent.strategy.parameters();
        
        // Get weighted priorities
        let food_priority = hunger_urgency * strategy.food_weight;
        let water_priority = thirst_urgency * strategy.water_weight;
        
        // If energy is very low, prioritize rest
        if agent.energy < 20.0 && rand::random::<f32>() < strategy.energy_conservation {
            return DecisionType::Rest;
        }
        
        // Exploration check
        if rand::random::<f32>() < strategy.exploration_tendency {
            return DecisionType::Explore;
        }
        
        // Determine what to prioritize
        if food_priority > water_priority {
            DecisionType::SeekFood
        } else {
            DecisionType::SeekWater
        }
    }
    
    /// Convert a decision type into an actual move
    pub fn convert_to_move(
        decision: DecisionType,
        agent: &Agent,
        resources: &VisibleResources,
        time_of_day: TimeOfDay,
    ) -> Option<Direction> {
        match decision {
            DecisionType::SeekFood => {
                if resources.food.is_empty() {
                    return Self::fallback_decision(agent, resources, time_of_day);
                }
                
                // Find the closest food
                let mut targets = resources.food.clone();
                targets.sort_by(|a, b| a.distance.cmp(&b.distance));
                
                let target = &targets[0];
                Self::calculate_direction_to_target(agent.x, agent.y, target.x, target.y)
            },
            DecisionType::SeekWater => {
                if resources.water.is_empty() {
                    return Self::fallback_decision(agent, resources, time_of_day);
                }
                
                // Find the closest water
                let mut targets = resources.water.clone();
                targets.sort_by(|a, b| a.distance.cmp(&b.distance));
                
                let target = &targets[0];
                Self::calculate_direction_to_target(agent.x, agent.y, target.x, target.y)
            },
            DecisionType::Rest => {
                // No movement
                None
            },
            DecisionType::Explore => {
                // Random direction
                Some(Direction::random())
            },
            DecisionType::Conservative => {
                // If at night and conservative, stay put more often
                if time_of_day == TimeOfDay::Night && rand::random::<f32>() < 0.7 {
                    None
                } else {
                    // Occasional small random move
                    Some(Direction::random())
                }
            },
        }
    }
    
    /// Fallback decision when the primary choice isn't available
    fn fallback_decision(
        agent: &Agent,
        resources: &VisibleResources,
        time_of_day: TimeOfDay,
    ) -> Option<Direction> {
        // If anything is visible, go for the closest resource
        if !resources.food.is_empty() {
            let mut food = resources.food.clone();
            food.sort_by(|a, b| a.distance.cmp(&b.distance));
            return Self::calculate_direction_to_target(agent.x, agent.y, food[0].x, food[0].y);
        }
        
        if !resources.water.is_empty() {
            let mut water = resources.water.clone();
            water.sort_by(|a, b| a.distance.cmp(&b.distance));
            return Self::calculate_direction_to_target(agent.x, agent.y, water[0].x, water[0].y);
        }
        
        // If at night, be more conservative
        if time_of_day == TimeOfDay::Night && rand::random::<f32>() < agent.strategy.parameters().night_behavior {
            if rand::random::<f32>() < 0.7 {
                return None; // Stay put
            }
        }
        
        // Random exploration
        Some(Direction::random())
    }
    
    /// Calculate the direction to move towards a target
    fn calculate_direction_to_target(agent_x: usize, agent_y: usize, target_x: usize, target_y: usize) -> Option<Direction> {
        let dx = target_x as isize - agent_x as isize;
        let dy = target_y as isize - agent_y as isize;
        
        // No movement needed if already at target
        if dx == 0 && dy == 0 {
            return None;
        }
        
        // Prioritize the larger distance component
        if dx.abs() > dy.abs() {
            if dx > 0 {
                Some(Direction::East)
            } else {
                Some(Direction::West)
            }
        } else {
            if dy > 0 {
                Some(Direction::South)
            } else {
                Some(Direction::North)
            }
        }
    }
}