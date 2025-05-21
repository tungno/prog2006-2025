use std::collections::HashMap;
use std::fmt;
use crate::agents::Agent;
use crate::environment::{TimeOfDay, grid::Grid, cell::CellType};
use crate::evolution::EvolutionMetrics;

/// A single data point for statistics
#[derive(Debug, Clone)]
pub struct StatPoint {
    pub tick: usize,
    pub population: usize,
    pub avg_health: f32,
    pub avg_hunger: f32,
    pub avg_thirst: f32,
    pub avg_energy: f32,
    pub avg_age: f32,
    pub food_count: usize,
    pub water_count: usize,
    pub time_of_day: TimeOfDay,
    pub strategy_distribution: HashMap<String, usize>,
}

/// Collects and stores statistics about the simulation
#[derive(Debug)]
pub struct StatisticsCollector {
    pub history: Vec<StatPoint>,
    pub total_ticks: usize,
    pub collection_interval: usize,
    pub evolution_metrics: EvolutionMetrics,
}

impl StatisticsCollector {
    /// Create a new statistics collector
    pub fn new(collection_interval: usize) -> Self {
        Self {
            history: Vec::new(),
            total_ticks: 0,
            collection_interval,
            evolution_metrics: EvolutionMetrics::new(),
        }
    }
    
    /// Update statistics with current simulation state
    pub fn update(
        &mut self, 
        agents: &[Agent], 
        dead_agents: &[Agent],
        grid: &Grid, 
        time_of_day: TimeOfDay
    ) {
        self.total_ticks += 1;
        
        // Update evolution metrics
        self.evolution_metrics.update(agents, dead_agents);
        
        // Only collect stats at specified intervals
        if self.total_ticks % self.collection_interval != 0 {
            return;
        }
        
        let living_agents: Vec<&Agent> = agents.iter()
            .filter(|a| a.alive)
            .collect();
        
        // Skip if no living agents
        if living_agents.is_empty() {
            return;
        }
        
        // Calculate averages
        let total_health: f32 = living_agents.iter().map(|a| a.health).sum();
        let total_hunger: f32 = living_agents.iter().map(|a| a.hunger).sum();
        let total_thirst: f32 = living_agents.iter().map(|a| a.thirst).sum();
        let total_energy: f32 = living_agents.iter().map(|a| a.energy).sum();
        let total_age: u32 = living_agents.iter().map(|a| a.age).sum();
        
        let avg_health = total_health / living_agents.len() as f32;
        let avg_hunger = total_hunger / living_agents.len() as f32;
        let avg_thirst = total_thirst / living_agents.len() as f32;
        let avg_energy = total_energy / living_agents.len() as f32;
        let avg_age = total_age as f32 / living_agents.len() as f32;
        
        // Count resources
        let food_count = grid.count_cells_of_type(CellType::Food);
        let water_count = grid.count_cells_of_type(CellType::Water);
        
        // Calculate strategy distribution
        let population_count = living_agents.len();
        let mut strategy_distribution = HashMap::new();
        for agent in &living_agents {
            *strategy_distribution.entry(agent.strategy_name.clone()).or_insert(0) += 1;
        }
        
        // Create the statistics point
        let point = StatPoint {
            tick: self.total_ticks,
            population: population_count,
            avg_health,
            avg_hunger,
            avg_thirst,
            avg_energy,
            avg_age,
            food_count,
            water_count,
            time_of_day,
            strategy_distribution,
        };
        
        self.history.push(point);
        
        // Limit history size to avoid excessive memory usage
        if self.history.len() > 1000 {
            self.history.remove(0);
        }
    }
    
    /// Reset statistics for a new simulation
    pub fn reset(&mut self) {
        self.history.clear();
        self.total_ticks = 0;
        self.evolution_metrics = EvolutionMetrics::new();
    }
    
    /// Get the most recent statistics point
    pub fn latest(&self) -> Option<&StatPoint> {
        self.history.last()
    }
    
    /// Get statistics from a specific tick
    pub fn get_at_tick(&self, tick: usize) -> Option<&StatPoint> {
        self.history.iter().find(|p| p.tick == tick)
    }
    
    /// Get the history for a specific metric
    pub fn get_history_for(&self, metric: StatisticType) -> Vec<(usize, f32)> {
        self.history.iter().map(|point| {
            let value = match metric {
                StatisticType::Population => point.population as f32,
                StatisticType::AvgHealth => point.avg_health,
                StatisticType::AvgHunger => point.avg_hunger,
                StatisticType::AvgThirst => point.avg_thirst,
                StatisticType::AvgEnergy => point.avg_energy,
                StatisticType::AvgAge => point.avg_age,
                StatisticType::FoodCount => point.food_count as f32,
                StatisticType::WaterCount => point.water_count as f32,
            };
            
            (point.tick, value)
        }).collect()
    }
}

/// Types of statistics that can be tracked
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum StatisticType {
    Population,
    AvgHealth,
    AvgHunger,
    AvgThirst,
    AvgEnergy,
    AvgAge,
    FoodCount,
    WaterCount,
}

impl fmt::Display for StatisticType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.label())
    }
}

impl StatisticType {
    /// Get a human-readable name for this statistic type
    pub fn label(&self) -> &'static str {
        match self {
            StatisticType::Population => "Population",
            StatisticType::AvgHealth => "Average Health",
            StatisticType::AvgHunger => "Average Hunger",
            StatisticType::AvgThirst => "Average Thirst",
            StatisticType::AvgEnergy => "Average Energy",
            StatisticType::AvgAge => "Average Age",
            StatisticType::FoodCount => "Food Count",
            StatisticType::WaterCount => "Water Count",
        }
    }
    
    /// Get all available statistic types
    pub fn all() -> Vec<StatisticType> {
        vec![
            StatisticType::Population,
            StatisticType::AvgHealth,
            StatisticType::AvgHunger,
            StatisticType::AvgThirst,
            StatisticType::AvgEnergy,
            StatisticType::AvgAge,
            StatisticType::FoodCount,
            StatisticType::WaterCount,
        ]
    }
}