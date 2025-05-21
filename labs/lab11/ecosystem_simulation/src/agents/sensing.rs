use crate::constants::SENSING_RADIUS;
use crate::environment::grid::Grid;
use crate::environment::cell::CellType;
use crate::heuristics::VisibleResources;

/// Helper for agents to sense their environment
pub struct Sensing;

impl Sensing {
    /// Scan the environment around a position for resources and obstacles
    pub fn scan_environment(grid: &Grid, x: usize, y: usize, radius: usize) -> VisibleResources {
        let mut resources = VisibleResources {
            food: Vec::new(),
            water: Vec::new(),
            obstacles: Vec::new(),
            agents: Vec::new(),
        };
        
        // Scan the area within the sensing radius
        for dy in -(radius as isize)..=(radius as isize) {
            for dx in -(radius as isize)..=(radius as isize) {
                let nx = x as isize + dx;
                let ny = y as isize + dy;
                
                // Skip if out of bounds
                if !grid.is_within_bounds(nx, ny) {
                    continue;
                }
                
                let nx = nx as usize;
                let ny = ny as usize;
                
                // Skip self
                if nx == x && ny == y {
                    continue;
                }
                
                // Calculate Manhattan distance
                let distance = dx.abs() as usize + dy.abs() as usize;
                
                // Skip if outside the radius
                if distance > radius {
                    continue;
                }
                
                if let Some(cell) = grid.get_cell(nx, ny) {
                    match cell.cell_type {
                        CellType::Food => resources.food.push(crate::heuristics::VisibleItem { 
                            x: nx, y: ny, distance 
                        }),
                        CellType::Water => resources.water.push(crate::heuristics::VisibleItem { 
                            x: nx, y: ny, distance 
                        }),
                        CellType::Obstacle => resources.obstacles.push(crate::heuristics::VisibleItem { 
                            x: nx, y: ny, distance 
                        }),
                        CellType::Agent => resources.agents.push(crate::heuristics::VisibleItem { 
                            x: nx, y: ny, distance 
                        }),
                        _ => {}
                    }
                }
            }
        }
        
        resources
    }
    
    /// Check if a given position has a visible resource
    pub fn has_resource_in_sight(grid: &Grid, x: usize, y: usize, radius: usize) -> bool {
        let resources = Self::scan_environment(grid, x, y, radius);
        !resources.food.is_empty() || !resources.water.is_empty()
    }
    
    /// Find the closest resource of a specific type
    pub fn find_closest_resource(
        grid: &Grid, 
        x: usize, 
        y: usize, 
        radius: usize,
        resource_type: CellType
    ) -> Option<(usize, usize, usize)> {
        let resources = Self::scan_environment(grid, x, y, radius);
        
        let items = match resource_type {
            CellType::Food => &resources.food,
            CellType::Water => &resources.water,
            _ => return None,
        };
        
        if items.is_empty() {
            return None;
        }
        
        // Find the closest
        let closest = items.iter()
            .min_by_key(|item| item.distance)?;
            
        Some((closest.x, closest.y, closest.distance))
    }
}