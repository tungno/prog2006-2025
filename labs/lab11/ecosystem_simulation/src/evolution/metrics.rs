use std::collections::HashMap;
use crate::agents::Agent;

/// Tracks the performance of different strategies
#[derive(Debug, Clone)]
pub struct StrategyPerformance {
    pub name: String,
    pub count: usize,
    pub avg_age: f32,
    pub avg_health: f32,
    pub max_age: u32,
    pub avg_fitness: f32,
}

/// Metrics tracking for evolution
#[derive(Debug, Clone)]
pub struct EvolutionMetrics {
    pub generation: usize,
    pub total_agents_created: usize,
    pub total_agents_died: usize,
    pub strategy_performance: HashMap<String, StrategyPerformance>,
    pub average_lifespan: f32,
    pub average_fitness: f32,
    pub best_fitness: f32,
    pub best_strategy: Option<String>,
}

impl EvolutionMetrics {
    /// Create new empty metrics
    pub fn new() -> Self {
        Self {
            generation: 1,
            total_agents_created: 0,
            total_agents_died: 0,
            strategy_performance: HashMap::new(),
            average_lifespan: 0.0,
            average_fitness: 0.0,
            best_fitness: 0.0,
            best_strategy: None,
        }
    }
    
    /// Reset metrics for a new generation
    pub fn next_generation(&mut self) {
        self.generation += 1;
        self.strategy_performance.clear();
    }
    
    /// Update metrics with the current agents
    pub fn update(&mut self, living_agents: &[Agent], dead_agents: &[Agent]) {
        // Count newly created agents
        let current_total = living_agents.len() + dead_agents.len();
        if current_total > self.total_agents_created {
            self.total_agents_created = current_total;
        }
        
        // Count newly dead agents
        self.total_agents_died = dead_agents.len();
        
        // Calculate average lifespan of dead agents
        if !dead_agents.is_empty() {
            let total_lifespan: u32 = dead_agents.iter().map(|a| a.age).sum();
            self.average_lifespan = total_lifespan as f32 / dead_agents.len() as f32;
        }
        
        // Calculate strategy performance
        self.update_strategy_performance(living_agents);
        
        // Calculate overall fitness metrics
        self.update_fitness_metrics(living_agents);
    }
    
    /// Update strategy performance metrics
    fn update_strategy_performance(&mut self, agents: &[Agent]) {
        let mut strategy_data: HashMap<String, Vec<&Agent>> = HashMap::new();
        
        // Group agents by strategy
        for agent in agents {
            strategy_data
                .entry(agent.strategy_name.clone())
                .or_insert_with(Vec::new)
                .push(agent);
        }
        
        // Calculate performance metrics for each strategy
        for (name, agents) in strategy_data {
            let count = agents.len();
            let total_age: u32 = agents.iter().map(|a| a.age).sum();
            let total_health: f32 = agents.iter().map(|a| a.health).sum();
            let max_age = agents.iter().map(|a| a.age).max().unwrap_or(0);
            let total_fitness: f32 = agents.iter().map(|a| a.get_fitness_score()).sum();
            
            let performance = StrategyPerformance {
                name: name.clone(),
                count,
                avg_age: total_age as f32 / count as f32,
                avg_health: total_health / count as f32,
                max_age,
                avg_fitness: total_fitness / count as f32,
            };
            
            self.strategy_performance.insert(name, performance);
        }
    }
    
    /// Update overall fitness metrics
    fn update_fitness_metrics(&mut self, agents: &[Agent]) {
        if agents.is_empty() {
            self.average_fitness = 0.0;
            self.best_fitness = 0.0;
            self.best_strategy = None;
            return;
        }
        
        let total_fitness: f32 = agents.iter().map(|a| a.get_fitness_score()).sum();
        self.average_fitness = total_fitness / agents.len() as f32;
        
        // Find the best agent
        if let Some(best_agent) = agents.iter().max_by(|a, b| {
            a.get_fitness_score().partial_cmp(&b.get_fitness_score()).unwrap()
        }) {
            self.best_fitness = best_agent.get_fitness_score();
            self.best_strategy = Some(best_agent.strategy_name.clone());
        }
    }
}