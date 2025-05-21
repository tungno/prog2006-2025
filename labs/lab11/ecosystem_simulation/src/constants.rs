// Grid parameters
pub const GRID_SIZE: usize = 50; // Can be increased to 100 for final implementation
pub const CELL_SIZE: f32 = 10.0; // Cell size in pixels

// Entity generation parameters
pub const INITIAL_AGENTS: usize = 20;
pub const INITIAL_FOOD: usize = 40;
pub const INITIAL_WATER: usize = 30;
pub const INITIAL_OBSTACLES: usize = 60;

// Agent parameters
pub const SENSING_RADIUS: usize = 10;
pub const MAX_AGE: u32 = 1000;

// Resource regeneration rates
pub const FOOD_REGENERATION_RATE: f32 = 0.05;
pub const WATER_REGENERATION_RATE: f32 = 0.03;

// Day/Night cycle parameters
pub const DAY_LENGTH: u32 = 200; // Ticks per full cycle
pub const DAY_NIGHT_TRANSITION: u32 = DAY_LENGTH / 2;

// Evolution parameters
pub const EVOLUTION_THRESHOLD: f32 = 0.5; // Trigger evolution when population falls below this proportion
pub const MUTATION_RATE: f32 = 0.2;
pub const MUTATION_AMOUNT: f32 = 0.1;