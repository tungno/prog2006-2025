# Ecosystem Simulation

An agent-based ecosystem simulation where autonomous entities navigate a grid environment to find resources for survival, featuring evolvable decision-making strategies and natural selection.

## Project Overview

This Rust application simulates a grid-based ecosystem where agents must find food and water to survive. The key innovation is the implementation of customizable, evolvable decision-making strategies, enabling natural selection to occur as successful agents pass their behavioral parameters to offspring.

![Ecosystem Simulation](assets/simulation_screenshot.png)

## Key Features

- **Agent-Based Modeling**: Autonomous agents with needs (hunger, thirst, energy, health)
- **Parameterized Decision-Making**: Customizable strategies that determine agent behavior
- **Evolutionary Mechanics**: Natural selection through breeding of successful strategies
- **Interactive Visualization**: Real-time view of the simulation with agent tracking
- **Environmental Factors**: Day/night cycle affecting resource availability
- **Statistical Analysis**: Track population trends and strategy effectiveness

## Running the Simulation

```bash
# Navigate to the project directory
cd ecosystem_simulation

# Run the simulation
cargo run --release
```

## User Interface

The application features four main sections:

1. **Simulation View**: Grid environment with agents, food, water, and obstacles
2. **Controls Panel**: Adjust speed, population parameters, and resource rates
3. **Heuristic Editor**: Create and modify agent decision strategies
4. **Statistics**: View metrics like population, survival rates, and strategy performance

## How It Works

### Agent Design

Each agent maintains several vital attributes:
- **Hunger**: Increases over time, decreased by consuming food
- **Thirst**: Increases over time, decreased by consuming water
- **Energy**: Depleted by movement, replenished by rest
- **Health**: Decreases when hunger or thirst are high

Agents make decisions using a weighted parameter system:

```rust
pub struct StrategyParameters {
    pub food_weight: f32,         // Priority for finding food
    pub water_weight: f32,        // Priority for finding water
    pub energy_conservation: f32, // Tendency to preserve energy
    pub exploration_tendency: f32, // Willingness to explore
    pub night_behavior: f32,      // Cautiousness during night
}
```

### Evolution Process

When the population drops below a threshold, evolution occurs:

1. **Selection**: The most successful agents (based on age, health) become parents
2. **Crossover**: Strategy parameters from parents are combined
3. **Mutation**: Small random changes are introduced to maintain diversity
4. **Generation**: New agents are created with these evolved strategies

This process allows effective strategies to persist and improve over generations.

### Environment

The grid environment contains:
- **Food resources**: Replenish hunger when consumed
- **Water resources**: Replenish thirst when consumed
- **Obstacles**: Block agent movement
- **Day/night cycle**: Affects resource regeneration and agent behavior

## Implementation Architecture

The project follows a modular design with six core components:

1. **Agents Module**: Core agent behavior and sensing mechanisms
2. **Environment Module**: Grid representation and resource management
3. **Heuristics Module**: Decision-making strategies and parameters
4. **Evolution Module**: Genetic algorithm implementation
5. **Statistics Module**: Data collection and visualization
6. **UI Module**: Interactive user interface components

## Observations and Results

Through the simulation, several emergent behaviors can be observed:

- Initially random strategies evolve toward more balanced resource management
- Specialized strategies emerge in different environmental conditions
- Population stabilizes around carrying capacity of the environment
- Strategies adapt to changes in resource distribution

## Extensions and Future Work

- **Additional Factors**: Weather patterns, seasons, predator-prey relationships
- **Spatial Adaptation**: Regional specialization of strategies
- **Group Behavior**: Cooperation and competition between agents
- **Learning**: Individual strategy adaptation during an agent's lifetime

## Technologies Used

- **Rust**: Memory-safe systems programming language
- **Iced**: Pure Rust GUI library with canvas capabilities
- **Rand**: Random number generation for stochastic processes
- **Serde**: Serialization for strategy import/export

## Project Structure

```
src/
├── agents/       # Agent behavior and sensing
├── environment/  # Grid and resources
├── evolution/    # Genetic algorithms
├── heuristics/   # Decision strategies
├── statistics/   # Data collection
├── ui/           # User interface
├── simulation.rs # Main simulation loop
└── main.rs       # Application entry point
```

---

This project was created for PROG2006 (Advanced Programming) to demonstrate Rust's capabilities for complex domain modeling, trait-based polymorphism, and interactive visualization.