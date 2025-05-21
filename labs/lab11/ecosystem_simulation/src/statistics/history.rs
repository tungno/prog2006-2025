use std::collections::{VecDeque, HashMap};
use crate::agents::Agent;
use crate::statistics::collector::{StatPoint, StatisticType};
use crate::heuristics::StrategyParameters;

/// A data point for the historical data chart
#[derive(Debug, Clone)]
pub struct HistoryPoint {
    pub tick: usize,
    pub value: f32,
    pub label: String,
}

/// Historical data for charting
#[derive(Debug)]
pub struct HistoricalData {
    population: VecDeque<HistoryPoint>,
    avg_health: VecDeque<HistoryPoint>,
    resource_levels: VecDeque<HistoryPoint>, // Combined food and water
    strategy_distribution: VecDeque<HashMap<String, usize>>,
    selected_agent_history: HashMap<String, VecDeque<HistoryPoint>>,
    max_history_size: usize,
}

impl HistoricalData {
    /// Create a new historical data collection
    pub fn new(max_size: usize) -> Self {
        Self {
            population: VecDeque::with_capacity(max_size),
            avg_health: VecDeque::with_capacity(max_size),
            resource_levels: VecDeque::with_capacity(max_size),
            strategy_distribution: VecDeque::with_capacity(max_size),
            selected_agent_history: HashMap::new(),
            max_history_size: max_size,
        }
    }
    
    /// Update history with a new statistics point
    pub fn update(&mut self, point: &StatPoint) {
        // Add population data
        self.population.push_back(HistoryPoint {
            tick: point.tick,
            value: point.population as f32,
            label: format!("Tick {}", point.tick),
        });
        
        // Add health data
        self.avg_health.push_back(HistoryPoint {
            tick: point.tick,
            value: point.avg_health,
            label: format!("Tick {}", point.tick),
        });
        
        // Add resource data
        self.resource_levels.push_back(HistoryPoint {
            tick: point.tick,
            value: (point.food_count + point.water_count) as f32,
            label: format!("Tick {}", point.tick),
        });
        
        // Add strategy distribution
        self.strategy_distribution.push_back(point.strategy_distribution.clone());
        
        // Trim if needed
        self.trim_to_max_size();
    }
    
    /// Update history for a specific agent
    pub fn update_agent(&mut self, agent: &Agent) {
        if !agent.alive {
            return;
        }
        
        let id = &agent.id;
        let history = self.selected_agent_history
            .entry(id.clone())
            .or_insert_with(|| VecDeque::with_capacity(100));
            
        // Add the agent's current stats
        history.push_back(HistoryPoint {
            tick: agent.age as usize,
            value: agent.health,
            label: "Health".to_string(),
        });
        
        // Keep only recent history
        while history.len() > 100 {
            history.pop_front();
        }
    }
    
    /// Reset agent history when selecting a new agent
    pub fn select_agent(&mut self, agent_id: &str) {
        // Clear history for all other agents to save memory
        self.selected_agent_history.retain(|id, _| id == agent_id);
    }
    
    /// Trim all histories to max size
    fn trim_to_max_size(&mut self) {
        while self.population.len() > self.max_history_size {
            self.population.pop_front();
        }
        
        while self.avg_health.len() > self.max_history_size {
            self.avg_health.pop_front();
        }
        
        while self.resource_levels.len() > self.max_history_size {
            self.resource_levels.pop_front();
        }
        
        while self.strategy_distribution.len() > self.max_history_size {
            self.strategy_distribution.pop_front();
        }
    }
    
    /// Get population history for charting
    pub fn get_population_history(&self) -> Vec<HistoryPoint> {
        self.population.iter().cloned().collect()
    }
    
    /// Get health history for charting
    pub fn get_health_history(&self) -> Vec<HistoryPoint> {
        self.avg_health.iter().cloned().collect()
    }
    
    /// Get resource history for charting
    pub fn get_resource_history(&self) -> Vec<HistoryPoint> {
        self.resource_levels.iter().cloned().collect()
    }
    
    /// Get strategy distribution history
    pub fn get_strategy_distribution(&self) -> Vec<HashMap<String, usize>> {
        self.strategy_distribution.iter().cloned().collect()
    }
    
    /// Get history for a selected agent
    pub fn get_agent_history(&self, agent_id: &str) -> Option<Vec<HistoryPoint>> {
        self.selected_agent_history.get(agent_id)
            .map(|history| history.iter().cloned().collect())
    }
    
    /// Reset all history
    pub fn reset(&mut self) {
        self.population.clear();
        self.avg_health.clear();
        self.resource_levels.clear();
        self.strategy_distribution.clear();
        self.selected_agent_history.clear();
    }
}