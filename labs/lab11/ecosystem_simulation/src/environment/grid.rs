use crate::constants::{GRID_SIZE};
use crate::environment::cell::{Cell, CellType};
use rand::Rng;

/// Represents the grid environment containing all entities
#[derive(Debug, Clone)]
pub struct Grid {
    cells: Vec<Vec<Cell>>,
}

impl Grid {
    /// Create a new empty grid
    pub fn new() -> Self {
        let cells = vec![vec![Cell::new_empty(); GRID_SIZE]; GRID_SIZE];
        Self { cells }
    }

    /// Get a reference to a cell at the specified coordinates
    pub fn get_cell(&self, x: usize, y: usize) -> Option<&Cell> {
        if x < GRID_SIZE && y < GRID_SIZE {
            Some(&self.cells[y][x])
        } else {
            None
        }
    }

    /// Get a mutable reference to a cell at the specified coordinates
    pub fn get_cell_mut(&mut self, x: usize, y: usize) -> Option<&mut Cell> {
        if x < GRID_SIZE && y < GRID_SIZE {
            Some(&mut self.cells[y][x])
        } else {
            None
        }
    }

    /// Set a cell at the specified coordinates to a particular type
    pub fn set_cell(&mut self, x: usize, y: usize, cell_type: CellType) -> bool {
        if x < GRID_SIZE && y < GRID_SIZE {
            self.cells[y][x] = Cell::new(cell_type);
            true
        } else {
            false
        }
    }

    /// Set a cell at the specified coordinates to contain an agent
    pub fn set_agent(&mut self, x: usize, y: usize, agent_id: String) -> bool {
        if x < GRID_SIZE && y < GRID_SIZE {
            self.cells[y][x] = Cell::new_agent(agent_id);
            true
        } else {
            false
        }
    }

    /// Find a random empty cell in the grid
    pub fn find_random_empty_cell(&self) -> Option<(usize, usize)> {
        let mut rng = rand::thread_rng();
        let mut attempts = 0;
        let max_attempts = GRID_SIZE * GRID_SIZE;
        
        while attempts < max_attempts {
            let x = rng.gen_range(0..GRID_SIZE);
            let y = rng.gen_range(0..GRID_SIZE);
            
            if self.cells[y][x].is_empty() {
                return Some((x, y));
            }
            
            attempts += 1;
        }
        
        // If we've tried many times and still can't find an empty cell,
        // do a complete scan of the grid
        for y in 0..GRID_SIZE {
            for x in 0..GRID_SIZE {
                if self.cells[y][x].is_empty() {
                    return Some((x, y));
                }
            }
        }
        
        None // Grid is completely full
    }

    /// Place entities of a specific type randomly in the grid
    pub fn place_entities_randomly(&mut self, cell_type: CellType, count: usize) -> usize {
        let mut placed = 0;
        
        for _ in 0..count {
            if let Some((x, y)) = self.find_random_empty_cell() {
                self.set_cell(x, y, cell_type);
                placed += 1;
            } else {
                break; // No more empty cells
            }
        }
        
        placed
    }

    /// Count the number of cells of a specific type
    pub fn count_cells_of_type(&self, cell_type: CellType) -> usize {
        let mut count = 0;
        
        for row in &self.cells {
            for cell in row {
                if cell.cell_type == cell_type {
                    count += 1;
                }
            }
        }
        
        count
    }

    /// Clear all agents from the grid
    pub fn clear_agents(&mut self) {
        for y in 0..GRID_SIZE {
            for x in 0..GRID_SIZE {
                if self.cells[y][x].has_agent() {
                    self.cells[y][x] = Cell::new_empty();
                }
            }
        }
    }

    /// Check if coordinates are within grid bounds
    pub fn is_within_bounds(&self, x: isize, y: isize) -> bool {
        x >= 0 && x < GRID_SIZE as isize && y >= 0 && y < GRID_SIZE as isize
    }

    /// Get the size of the grid
    pub fn grid_size(&self) -> usize {
        GRID_SIZE
    }
    
    /// Check if a move to the specified coordinates is valid (within bounds and not an obstacle)
    pub fn is_valid_move(&self, x: isize, y: isize) -> bool {
        if !self.is_within_bounds(x, y) {
            return false;
        }
        
        let cell = &self.cells[y as usize][x as usize];
        !cell.is_obstacle()
    }
}