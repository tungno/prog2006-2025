use rand::Rng;
use crate::agents::Agent;
use crate::heuristics::{StrategyParameters, default_strategies};
use crate::constants::{MUTATION_RATE, MUTATION_AMOUNT};

/// Methods for breeding new strategies
pub struct Breeding;

impl Breeding {
    /// Create a new strategy through mutation
    pub fn mutate(parent: &StrategyParameters) -> StrategyParameters {
        parent.mutate(Some(MUTATION_RATE), Some(MUTATION_AMOUNT))
    }
    
    /// Create a new strategy through crossover of two parents
    pub fn crossover(parent1: &StrategyParameters, parent2: &StrategyParameters) -> StrategyParameters {
        parent1.crossover(parent2)
    }
    
    /// Generate a new strategy through mutation or crossover
    pub fn generate_new_strategy(parents: &[&Agent]) -> StrategyParameters {
        let mut rng = rand::thread_rng();
        
        if parents.is_empty() {
            // No parents available, return a random default strategy
            let default = default_strategies();
            let idx = rng.gen_range(0..default.len());
            return default[idx].clone();
        }
        
        if parents.len() == 1 || rng.gen_bool(0.3) {
            // Single parent or 30% chance of mutation
            let parent = parents[rng.gen_range(0..parents.len())];
            Self::mutate(parent.strategy.parameters())
        } else {
            // Crossover between two random parents
            let parent1 = parents[rng.gen_range(0..parents.len())];
            let parent2 = parents[rng.gen_range(0..parents.len())];
            
            // Make sure we don't crossover the same parent with itself
            if parent1.id == parent2.id && parents.len() > 1 {
                let parent2 = parents.iter()
                    .find(|p| p.id != parent1.id)
                    .map_or(parent1, |p| p);
                
                Self::crossover(parent1.strategy.parameters(), parent2.strategy.parameters())
            } else {
                Self::crossover(parent1.strategy.parameters(), parent2.strategy.parameters())
            }
        }
    }
    
    /// Generate offspring using the provided parents
    pub fn generate_offspring(
        parents: &[&Agent], 
        count: usize,
        elite_preservation: bool
    ) -> Vec<StrategyParameters> {
        let mut offspring = Vec::with_capacity(count);
        
        // First, preserve the elite parents' strategies if requested
        if elite_preservation && !parents.is_empty() {
            // Get top parents sorted by fitness
            let mut sorted_parents = parents.to_vec();
            sorted_parents.sort_by(|a, b| {
                b.get_fitness_score().partial_cmp(&a.get_fitness_score()).unwrap()
            });
            
            // Add the top parent strategies directly
            let elite_count = (count as f32 * 0.2) as usize; // 20% elites
            for i in 0..elite_count.min(sorted_parents.len()) {
                let mut strategy = sorted_parents[i].strategy.parameters().clone();
                strategy.name = format!("Elite_{}", strategy.name);
                offspring.push(strategy);
            }
        }
        
        // Fill the rest with new offspring
        while offspring.len() < count {
            offspring.push(Self::generate_new_strategy(parents));
        }
        
        offspring
    }
}