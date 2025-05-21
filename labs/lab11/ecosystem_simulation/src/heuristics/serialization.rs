use std::collections::HashMap;
use std::fs::{self, File};
use std::io::{self, Read, Write};
use std::path::Path;
use serde::{Serialize, Deserialize};
use crate::heuristics::parameters::{StrategyParameters, default_strategies};

/// Container for serializing and deserializing a collection of strategies
#[derive(Debug, Serialize, Deserialize)]
pub struct StrategiesCollection {
    pub strategies: HashMap<String, StrategyParameters>,
}

impl StrategiesCollection {
    /// Create a new empty collection
    pub fn new() -> Self {
        Self {
            strategies: HashMap::new(),
        }
    }

    /// Create a collection with default strategies
    pub fn with_defaults() -> Self {
        let mut strategies = HashMap::new();
        
        for strategy in default_strategies() {
            strategies.insert(strategy.name.clone(), strategy);
        }
        
        Self { strategies }
    }

    /// Add a strategy to the collection
    pub fn add_strategy(&mut self, strategy: StrategyParameters) {
        self.strategies.insert(strategy.name.clone(), strategy);
    }

    /// Remove a strategy from the collection
    pub fn remove_strategy(&mut self, name: &str) -> Option<StrategyParameters> {
        self.strategies.remove(name)
    }

    /// Get a strategy by name
    pub fn get_strategy(&self, name: &str) -> Option<&StrategyParameters> {
        self.strategies.get(name)
    }

    /// Update an existing strategy
    pub fn update_strategy(&mut self, strategy: StrategyParameters) -> bool {
        if self.strategies.contains_key(&strategy.name) {
            self.strategies.insert(strategy.name.clone(), strategy);
            true
        } else {
            false
        }
    }

    /// Get a list of strategy names
    pub fn get_strategy_names(&self) -> Vec<String> {
        self.strategies.keys().cloned().collect()
    }

    /// Get a clone of all strategies
    pub fn get_all_strategies(&self) -> Vec<StrategyParameters> {
        self.strategies.values().cloned().collect()
    }

    /// Save strategies to a file
    pub fn save_to_file<P: AsRef<Path>>(&self, path: P) -> io::Result<()> {
        let json = serde_json::to_string_pretty(self)?;
        let mut file = File::create(path)?;
        file.write_all(json.as_bytes())?;
        Ok(())
    }

    /// Load strategies from a file
    pub fn load_from_file<P: AsRef<Path>>(path: P) -> io::Result<Self> {
        let mut file = File::open(path)?;
        let mut contents = String::new();
        file.read_to_string(&mut contents)?;
        
        let collection: StrategiesCollection = serde_json::from_str(&contents)?;
        Ok(collection)
    }
}