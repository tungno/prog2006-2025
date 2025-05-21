use iced::Color;

/// Represents different types of cells in the grid
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CellType {
    Empty,
    Agent,
    Food,
    Water,
    Obstacle,
}

impl CellType {
    /// Get the color representation of a cell type for day time
    pub fn day_color(&self) -> Color {
        match self {
            CellType::Empty => Color::from_rgb(0.94, 0.94, 0.94),   // Light gray
            CellType::Agent => Color::from_rgb(1.0, 0.0, 0.0),      // Red (default, will be overridden)
            CellType::Food => Color::from_rgb(0.3, 0.8, 0.3),       // Green
            CellType::Water => Color::from_rgb(0.13, 0.59, 0.95),   // Blue
            CellType::Obstacle => Color::from_rgb(0.38, 0.38, 0.38), // Dark gray
        }
    }

    /// Get the color representation of a cell type for night time
    pub fn night_color(&self) -> Color {
        match self {
            CellType::Empty => Color::from_rgb(0.13, 0.13, 0.13),   // Dark gray
            CellType::Agent => Color::from_rgb(0.8, 0.0, 0.0),      // Darker red (default, will be overridden)
            CellType::Food => Color::from_rgb(0.16, 0.38, 0.18),    // Darker green
            CellType::Water => Color::from_rgb(0.05, 0.3, 0.53),    // Darker blue
            CellType::Obstacle => Color::from_rgb(0.19, 0.19, 0.19), // Very dark gray
        }
    }
}

/// Represents a cell in the grid
#[derive(Debug, Clone)]
pub struct Cell {
    pub cell_type: CellType,
    pub agent_id: Option<String>, // If the cell contains an agent, this is its ID
}

impl From<CellType> for Cell {
    fn from(cell_type: CellType) -> Self {
        Cell {
            cell_type,
            agent_id: None,
        }
    }
}

impl Cell {
    /// Create a new empty cell
    pub fn new_empty() -> Self {
        Self {
            cell_type: CellType::Empty,
            agent_id: None,
        }
    }

    /// Create a new cell with the specified type
    pub fn new(cell_type: CellType) -> Self {
        Self {
            cell_type,
            agent_id: None,
        }
    }

    /// Create a new cell containing an agent
    pub fn new_agent(agent_id: String) -> Self {
        Self {
            cell_type: CellType::Agent,
            agent_id: Some(agent_id),
        }
    }

    /// Check if the cell is empty
    pub fn is_empty(&self) -> bool {
        self.cell_type == CellType::Empty
    }

    /// Check if the cell contains an agent
    pub fn has_agent(&self) -> bool {
        self.cell_type == CellType::Agent
    }

    /// Check if the cell contains a resource (food or water)
    pub fn is_resource(&self) -> bool {
        self.cell_type == CellType::Food || self.cell_type == CellType::Water
    }

    /// Check if the cell contains an obstacle
    pub fn is_obstacle(&self) -> bool {
        self.cell_type == CellType::Obstacle
    }
}