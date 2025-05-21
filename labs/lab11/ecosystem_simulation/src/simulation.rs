use std::fs;
use std::collections::{HashMap, VecDeque};
use rand::Rng;

use crate::constants::{
    INITIAL_AGENTS, INITIAL_FOOD, INITIAL_WATER, INITIAL_OBSTACLES,
    GRID_SIZE, SENSING_RADIUS, EVOLUTION_THRESHOLD,
};
use crate::environment::{Environment, TimeOfDay, cell::CellType};
use crate::agents::{Agent, Position};
use crate::heuristics::{StrategyParameters, StrategiesCollection};
use crate::evolution::{EvolutionMetrics, SelectionMethod, Breeding};
use crate::statistics::StatisticsCollector;

/// The main simulation class that coordinates all systems
pub struct Simulation {
    pub environment: Environment,
    pub agents: Vec<Agent>,
    pub dead_agents: Vec<Agent>,
    pub strategies: StrategiesCollection,
    pub statistics: StatisticsCollector,
    pub evolution_metrics: EvolutionMetrics,
    pub running: bool,
    pub day_counter: u32,
    pub generation: usize,
    selected_agent_id: Option<String>,
    tracked_agent_ids: Vec<String>,
}

impl Simulation {
    /// Create a new simulation with initial setup
    pub fn new() -> Self {
        let mut environment = Environment::new();
        let strategies = StrategiesCollection::with_defaults();
        let mut agents = Vec::new();
        
        // Create initial agents
        for _ in 0..INITIAL_AGENTS {
            if let Some((x, y)) = environment.grid.find_random_empty_cell() {
                // Select a random strategy
                let strategy_names: Vec<String> = strategies.get_strategy_names();
                let strategy_name = {
                    let mut rng = rand::thread_rng();
                    let idx = rng.gen_range(0..strategy_names.len());
                    strategy_names[idx].clone()
                };
                
                if let Some(strategy_params) = strategies.get_strategy(&strategy_name) {
                    let agent = Agent::new(x, y, &strategy_name, strategy_params.clone());
                    let agent_id = agent.id.clone();
                    environment.grid.set_agent(x, y, agent_id);
                    agents.push(agent);
                }
            }
        }
        
        let statistics = StatisticsCollector::new(10); // Collect stats every 10 ticks
        let evolution_metrics = EvolutionMetrics::new();
        
        Self {
            environment,
            agents,
            dead_agents: Vec::new(),
            strategies,
            statistics,
            evolution_metrics,
            running: false,
            day_counter: 0,
            generation: 1,
            selected_agent_id: None,
            tracked_agent_ids: Vec::new(),
        }
    }
    
    /// Update the simulation for one step
    pub fn update(&mut self) {
        if !self.running {
            return;
        }
        
        // Update day/night cycle
        self.day_counter = (self.day_counter + 1) % crate::constants::DAY_LENGTH;
        self.environment.update_time_of_day(self.day_counter, crate::constants::DAY_LENGTH);
        
        // Clear all agents from the grid before updating
        self.environment.grid.clear_agents();
        
        // Update each agent
        for agent in &mut self.agents {
            if agent.alive {
                agent.update(&mut self.environment.grid, self.environment.time_of_day);
            }
        }
        
        // Regenerate resources
        self.environment.regenerate_resources();
        
        // Check if any agents have died
        let mut new_dead = Vec::new();
        for i in (0..self.agents.len()).rev() {
            if !self.agents[i].alive {
                let dead_agent = self.agents.remove(i);
                new_dead.push(dead_agent);
            }
        }
        
        // Add any newly dead agents to the dead agents list
        self.dead_agents.extend(new_dead);
        
        // Check if evolution should be triggered
        let alive_count = self.alive_agents_count();
        let threshold = (INITIAL_AGENTS as f32 * EVOLUTION_THRESHOLD) as usize;
        
        if alive_count <= threshold {
            self.evolve_population();
        }
        
        // Update statistics
        self.statistics.update(&self.agents, &self.dead_agents, &self.environment.grid, self.environment.time_of_day);
    }
    
    /// Get the number of alive agents
    pub fn alive_agents_count(&self) -> usize {
        self.agents.iter().filter(|a| a.alive).count()
    }
    
    /// Get the average health of alive agents
    pub fn average_health(&self) -> f32 {
        let alive_agents = self.agents.iter().filter(|a| a.alive).collect::<Vec<_>>();
        if alive_agents.is_empty() {
            return 0.0;
        }
        
        alive_agents.iter().map(|a| a.health).sum::<f32>() / alive_agents.len() as f32
    }
    
    /// Get the average age of alive agents
    pub fn average_age(&self) -> f32 {
        let alive_agents = self.agents.iter().filter(|a| a.alive).collect::<Vec<_>>();
        if alive_agents.is_empty() {
            return 0.0;
        }
        
        alive_agents.iter().map(|a| a.age as f32).sum::<f32>() / alive_agents.len() as f32
    }
    
    /// Find an agent by ID
    pub fn find_agent(&self, id: &str) -> Option<&Agent> {
        self.agents.iter().find(|a| a.id == id)
    }
    
    /// Set the food regeneration rate
    pub fn set_food_regeneration_rate(&mut self, rate: f32) {
        let water_rate = self.environment.resource_manager.water_regeneration_rate();
        self.environment.resource_manager.update_rates(rate, water_rate);
    }
    
    /// Set the water regeneration rate
    pub fn set_water_regeneration_rate(&mut self, rate: f32) {
        let food_rate = self.environment.resource_manager.food_regeneration_rate();
        self.environment.resource_manager.update_rates(food_rate, rate);
    }
    
    /// Select an agent at the specified position
    pub fn select_agent_at(&mut self, x: usize, y: usize) {
        if let Some(cell) = self.environment.grid.get_cell(x, y) {
            if cell.cell_type == CellType::Agent {
                self.selected_agent_id = cell.agent_id.clone();
            }
        }
    }
    
    /// Get the currently selected agent
    pub fn selected_agent(&self) -> Option<&Agent> {
        if let Some(id) = &self.selected_agent_id {
            self.find_agent(id)
        } else {
            None
        }
    }
    
    /// Track an agent by ID
    pub fn track_agent(&mut self, id: &str) {
        if !self.tracked_agent_ids.contains(&id.to_string()) {
            self.tracked_agent_ids.push(id.to_string());
            
            // Limit the number of tracked agents
            if self.tracked_agent_ids.len() > 5 {
                self.tracked_agent_ids.remove(0);
            }
        }
    }
    
    /// Clear all tracked agents
    pub fn clear_tracked_agents(&mut self) {
        self.tracked_agent_ids.clear();
    }
    
    /// Get the currently tracked agents
    pub fn tracked_agents(&self) -> Vec<&Agent> {
        self.tracked_agent_ids.iter()
            .filter_map(|id| self.find_agent(id))
            .collect()
    }
    
    /// Trigger evolution manually
    pub fn trigger_evolution(&mut self) {
        self.evolve_population();
    }
    
    /// Evolve the agent population
    fn evolve_population(&mut self) {
        // Clear all agents from the grid
        self.environment.grid.clear_agents();
        
        let alive_agents: Vec<&Agent> = self.agents.iter().filter(|a| a.alive).collect();
        
        if alive_agents.is_empty() {
            // Complete reset if all agents died
            *self = Self::new();
            return;
        }
        
        // Use selection to choose parents
        let selection = SelectionMethod::Elitism(5.min(alive_agents.len()));
        // Convert Vec<&Agent> to Vec<Agent> for selection
        let alive_agent_refs: Vec<Agent> = alive_agents.iter().map(|&a| a.clone()).collect();
        let parents = selection.select(&alive_agent_refs);
        
        // Generate offspring strategies
        let offspring_strategies = Breeding::generate_offspring(
            &parents,
            INITIAL_AGENTS,
            true // Preserve elite parents
        );
        
        // Create new generation of agents
        let mut new_agents = Vec::with_capacity(INITIAL_AGENTS);
        
        for strategy_params in offspring_strategies {
            if let Some((x, y)) = self.environment.grid.find_random_empty_cell() {
                let strategy_name = strategy_params.name.clone();
                let agent = Agent::new(x, y, &strategy_name, strategy_params);
                let agent_id = agent.id.clone();
                self.environment.grid.set_agent(x, y, agent_id);
                new_agents.push(agent);
            }
        }
        
        // Update agents and generation
        self.agents = new_agents;
        self.generation += 1;
        self.evolution_metrics.next_generation();
        
        // Clear selection and tracking
        self.selected_agent_id = None;
        self.tracked_agent_ids.clear();
    }
    
    /// Create a new strategy
    pub fn create_strategy(&mut self, name: &str) {
        if name.is_empty() {
            return;
        }
        
        let strategy = StrategyParameters::new(name);
        self.strategies.add_strategy(strategy);
    }
    
    /// Delete a strategy
    pub fn delete_strategy(&mut self, name: &str) {
        // Don't allow deleting if it's the last strategy
        if self.strategies.get_strategy_names().len() <= 1 {
            return;
        }
        
        self.strategies.remove_strategy(name);
    }
    
    /// Get a strategy by name
    pub fn get_strategy(&self, name: &str) -> Option<StrategyParameters> {
        self.strategies.get_strategy(name).cloned()
    }
    
    /// Update a strategy
    pub fn update_strategy(&mut self, params: StrategyParameters) {
        self.strategies.update_strategy(params);
    }
    
    /// Import strategies
    pub fn import_strategies(&mut self, strategies: HashMap<String, StrategyParameters>) {
        let mut new_strategies = StrategiesCollection::new();
        
        for (_name, strategy) in strategies {
            new_strategies.add_strategy(strategy);
        }
        
        // If the imported collection is empty, don't replace the current one
        if !new_strategies.get_strategy_names().is_empty() {
            self.strategies = new_strategies;
        }
    }
    
    /// Export strategies to a file
    pub fn export_strategies(&self) -> std::io::Result<()> {
        self.strategies.save_to_file("ecosystem-strategies.json")
    }
    
    /// Check if the simulation is running
    pub fn is_running(&self) -> bool {
        self.running
    }
    
    /// Set the running state
    pub fn set_running(&mut self, running: bool) {
        self.running = running;
    }
}