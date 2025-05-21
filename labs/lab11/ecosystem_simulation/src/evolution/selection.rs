use crate::agents::Agent;
use crate::heuristics::StrategyParameters;

/// Selection methods for evolution
pub enum SelectionMethod {
    /// Top performers based on fitness
    Elitism(usize),
    /// Random selection weighted by fitness
    RouletteWheel(usize),
    /// Tournament selection
    Tournament { size: usize, winners: usize },
}

impl SelectionMethod {
    /// Select agents for breeding based on the chosen method
    pub fn select<'a>(&self, agents: &'a [Agent]) -> Vec<&'a Agent> {
        match self {
            SelectionMethod::Elitism(count) => {
                self.elitism_selection(agents, *count)
            },
            SelectionMethod::RouletteWheel(count) => {
                self.roulette_wheel_selection(agents, *count)
            },
            SelectionMethod::Tournament { size, winners } => {
                self.tournament_selection(agents, *size, *winners)
            },
        }
    }
    
    /// Select the top performing agents
    fn elitism_selection<'a>(&self, agents: &'a [Agent], count: usize) -> Vec<&'a Agent> {
        let mut sorted_agents: Vec<&Agent> = agents.iter().collect();
        
        // Sort by fitness in descending order
        sorted_agents.sort_by(|a, b| {
            b.get_fitness_score().partial_cmp(&a.get_fitness_score()).unwrap()
        });
        
        // Take the top performers
        sorted_agents.truncate(count.min(sorted_agents.len()));
        sorted_agents
    }
    
    /// Roulette wheel selection (weighted random)
    fn roulette_wheel_selection<'a>(&self, agents: &'a [Agent], count: usize) -> Vec<&'a Agent> {
        use rand::Rng;
        let mut selected = Vec::new();
        
        if agents.is_empty() {
            return selected;
        }
        
        // Calculate total fitness
        let total_fitness: f32 = agents.iter()
            .map(|agent| agent.get_fitness_score().max(0.0))
            .sum();
        
        if total_fitness <= 0.0 {
            // Fallback to random selection if all fitness scores are <= 0
            let mut rng = rand::thread_rng();
            for _ in 0..count.min(agents.len()) {
                let idx = rng.gen_range(0..agents.len());
                selected.push(&agents[idx]);
            }
            return selected;
        }
        
        // Perform selection
        let mut rng = rand::thread_rng();
        for _ in 0..count.min(agents.len()) {
            let mut spin = rng.gen::<f32>() * total_fitness;
            let mut selected_agent = &agents[0];
            
            for agent in agents {
                let fitness = agent.get_fitness_score().max(0.0);
                if spin <= fitness {
                    selected_agent = agent;
                    break;
                }
                spin -= fitness;
            }
            
            selected.push(selected_agent);
        }
        
        selected
    }
    
    /// Tournament selection
    fn tournament_selection<'a>(&self, agents: &'a [Agent], size: usize, winners: usize) -> Vec<&'a Agent> {
        use rand::Rng;
        let mut selected = Vec::new();
        let mut rng = rand::thread_rng();
        
        if agents.is_empty() || size == 0 {
            return selected;
        }
        
        // Run tournaments until we have enough winners
        while selected.len() < winners.min(agents.len()) {
            // Select random contestants
            let tournament_size = size.min(agents.len());
            let mut contestants = Vec::with_capacity(tournament_size);
            
            while contestants.len() < tournament_size {
                let idx = rng.gen_range(0..agents.len());
                let contestant = &agents[idx];
                
                // Avoid duplicates
                if !contestants.contains(&contestant) {
                    contestants.push(contestant);
                }
            }
            
            // Sort by fitness
            contestants.sort_by(|a, b| {
                b.get_fitness_score().partial_cmp(&a.get_fitness_score()).unwrap()
            });
            
            // Add the winner
            selected.push(contestants[0]);
        }
        
        selected
    }
}