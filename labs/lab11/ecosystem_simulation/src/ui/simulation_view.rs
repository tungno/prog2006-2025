use iced::{
    widget::{Button, Canvas, Column, Container, Row, Scrollable, Text},
    Element, Length, Color, Rectangle, Point, Theme, Size, mouse,
};
use iced::widget::canvas::{self, Frame, Path, Stroke, LineCap, LineJoin, LineDash};

use crate::agents::Agent;
use crate::environment::{TimeOfDay, cell::CellType};
use crate::simulation::Simulation;
use crate::heuristics::{StrategyParameters, StrategiesCollection};
use crate::constants::{GRID_SIZE, CELL_SIZE, SENSING_RADIUS};
use crate::ui::Message;

/// Enumeration of heatmap types
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum HeatmapType {
    Food,
    Water,
}

/// The UI component for visualizing the grid and agents
pub struct SimulationView {
    grid_size: usize,
    cell_size: u16,
    show_sensing_radius: bool,
    show_agent_paths: bool,
    show_heatmap: bool,
    heatmap_type: HeatmapType,
    grid_state: Vec<Vec<CellState>>,
    selected_agent: Option<Agent>,
    tracked_agents: Vec<Agent>,
    time_of_day: TimeOfDay,
}

/// State of a grid cell for rendering
#[derive(Debug, Clone)]
struct CellState {
    cell_type: CellType,
    agent_id: Option<String>,
    agent_color: Option<Color>,
    visited_count: usize,
}

impl SimulationView {
    /// Create a new simulation view
    pub fn new(grid_size: usize, cell_size: u16) -> Self {
        let grid_state = vec![vec![CellState {
            cell_type: CellType::Empty,
            agent_id: None,
            agent_color: None,
            visited_count: 0,
        }; grid_size]; grid_size];
        
        Self {
            grid_size,
            cell_size,
            show_sensing_radius: false,
            show_agent_paths: false,
            show_heatmap: false,
            heatmap_type: HeatmapType::Food,
            grid_state,
            selected_agent: None,
            tracked_agents: Vec::new(),
            time_of_day: TimeOfDay::Day,
        }
    }
    
    /// Update the view from the current simulation state
    pub fn update_from_simulation(&mut self, simulation: &Simulation) {
        // Update time of day
        self.time_of_day = simulation.environment.time_of_day;
        
        // Update grid state
        for y in 0..self.grid_size {
            for x in 0..self.grid_size {
                if let Some(cell) = simulation.environment.grid.get_cell(x, y) {
                    self.grid_state[y][x].cell_type = cell.cell_type;
                    self.grid_state[y][x].agent_id = cell.agent_id.clone();
                    
                    // Find the agent color if this is an agent cell
                    if cell.has_agent() {
                        if let Some(agent_id) = &cell.agent_id {
                            if let Some(agent) = simulation.find_agent(agent_id) {
                                self.grid_state[y][x].agent_color = Some(agent.color);
                            }
                        }
                    } else {
                        self.grid_state[y][x].agent_color = None;
                    }
                }
            }
        }
        
        // Update visited counts for heatmap
        if self.show_heatmap {
            // Reset counts
            for y in 0..self.grid_size {
                for x in 0..self.grid_size {
                    self.grid_state[y][x].visited_count = 0;
                }
            }
            
            // Count agent visits
            for agent in &simulation.agents {
                if agent.alive {
                    for pos in &agent.movement_history {
                        if pos.x < self.grid_size && pos.y < self.grid_size {
                            self.grid_state[pos.y][pos.x].visited_count += 1;
                        }
                    }
                }
            }
        }
        
        // Update selected agent
        if let Some(selected) = &self.selected_agent {
            if let Some(updated) = simulation.find_agent(&selected.id) {
                self.selected_agent = Some(updated.clone());
            } else {
                // Agent no longer exists
                self.selected_agent = None;
            }
        }
        
        // Update tracked agents
        self.tracked_agents.clear();
        for tracked in simulation.tracked_agents() {
            self.tracked_agents.push(tracked.clone());
        }
    }
    
    /// Update the selected agent
    pub fn update_selected_agent(&mut self, agent: Option<&Agent>) {
        self.selected_agent = agent.cloned();
    }
    
    /// Update the tracked agents
    pub fn update_tracked_agents(&mut self, agents: Vec<&Agent>) {
        self.tracked_agents = agents.iter().map(|a| (*a).clone()).collect();
    }
    
    /// Toggle the sensing radius display
    pub fn toggle_show_sensing_radius(&mut self) {
        self.show_sensing_radius = !self.show_sensing_radius;
    }
    
    /// Set the sensing radius display state
    pub fn set_show_sensing_radius(&mut self, show: bool) {
        self.show_sensing_radius = show;
    }
    
    /// Get sensing radius display state
    pub fn is_show_sensing_radius(&self) -> bool {
        self.show_sensing_radius
    }
    
    /// Toggle the agent paths display
    pub fn toggle_show_agent_paths(&mut self) {
        self.show_agent_paths = !self.show_agent_paths;
    }
    
    /// Set the agent paths display state
    pub fn set_show_agent_paths(&mut self, show: bool) {
        self.show_agent_paths = show;
    }
    
    /// Get agent paths display state
    pub fn is_show_agent_paths(&self) -> bool {
        self.show_agent_paths
    }
    
    /// Toggle the heatmap display
    pub fn toggle_show_heatmap(&mut self) {
        self.show_heatmap = !self.show_heatmap;
    }
    
    /// Set the heatmap display state
    pub fn set_show_heatmap(&mut self, show: bool) {
        self.show_heatmap = show;
    }
    
    /// Get heatmap display state
    pub fn is_show_heatmap(&self) -> bool {
        self.show_heatmap
    }
    
    /// Set the heatmap type
    pub fn set_heatmap_type(&mut self, heatmap_type: HeatmapType) {
        self.heatmap_type = heatmap_type;
    }
    
    /// Get the current heatmap type
    pub fn get_heatmap_type(&self) -> HeatmapType {
        self.heatmap_type
    }
    
    /// Get the color for a cell
    fn get_cell_color(&self, cell_type: CellType, agent_color: Option<Color>) -> Color {
        match cell_type {
            CellType::Empty => match self.time_of_day {
                TimeOfDay::Day => Color::from_rgb(0.94, 0.94, 0.94),
                TimeOfDay::Night => Color::from_rgb(0.13, 0.13, 0.13),
            },
            CellType::Agent => agent_color.unwrap_or_else(|| match self.time_of_day {
                TimeOfDay::Day => Color::from_rgb(1.0, 0.0, 0.0),
                TimeOfDay::Night => Color::from_rgb(0.8, 0.0, 0.0),
            }),
            CellType::Food => match self.time_of_day {
                TimeOfDay::Day => Color::from_rgb(0.3, 0.8, 0.3),
                TimeOfDay::Night => Color::from_rgb(0.16, 0.38, 0.18),
            },
            CellType::Water => match self.time_of_day {
                TimeOfDay::Day => Color::from_rgb(0.13, 0.59, 0.95),
                TimeOfDay::Night => Color::from_rgb(0.05, 0.3, 0.53),
            },
            CellType::Obstacle => match self.time_of_day {
                TimeOfDay::Day => Color::from_rgb(0.38, 0.38, 0.38),
                TimeOfDay::Night => Color::from_rgb(0.19, 0.19, 0.19),
            },
        }
    }
    
    /// Get the opacity for a cell based on heatmap settings
    fn get_cell_opacity(&self, x: usize, y: usize) -> f32 {
        if !self.show_heatmap {
            return 1.0;
        }
        
        match self.heatmap_type {
            HeatmapType::Food => {
                let visits = self.grid_state[y][x].visited_count;
                f32::min(0.3 + (visits as f32 * 0.2), 1.0)
            },
            HeatmapType::Water => {
                // Find the closest water
                let mut min_distance = usize::MAX;
                
                for y2 in 0..self.grid_size {
                    for x2 in 0..self.grid_size {
                        if self.grid_state[y2][x2].cell_type == CellType::Water {
                            let distance = ((x as isize - x2 as isize).abs() + 
                                           (y as isize - y2 as isize).abs()) as usize;
                            min_distance = min_distance.min(distance);
                        }
                    }
                }
                
                if min_distance == usize::MAX {
                    0.3 // No water on the map
                } else {
                    // Normalize and invert
                    let normalized = f32::min(min_distance as f32 / SENSING_RADIUS as f32, 1.0);
                    f32::max(0.3, 1.0 - normalized)
                }
            }
        }
    }
    
    /// View the simulation grid
    pub fn view(&self) -> Element<Message> {
        // Pre-compute all cell colors and opacities
        let mut colors = vec![vec![Color::WHITE; self.grid_size]; self.grid_size];
        let mut opacities = vec![vec![1.0f32; self.grid_size]; self.grid_size];
        
        for y in 0..self.grid_size {
            for x in 0..self.grid_size {
                let cell = &self.grid_state[y][x];
                colors[y][x] = self.get_cell_color(cell.cell_type, cell.agent_color);
                opacities[y][x] = self.get_cell_opacity(x, y);
            }
        }
        
        let canvas = GridCanvas {
            grid_size: self.grid_size,
            cell_size: self.cell_size,
            grid_state: &self.grid_state,
            show_sensing_radius: self.show_sensing_radius,
            show_agent_paths: self.show_agent_paths,
            selected_agent: self.selected_agent.as_ref(),
            tracked_agents: &self.tracked_agents,
            time_of_day: self.time_of_day,
            cell_colors: colors,
            cell_opacities: opacities,
        };
        
        // Create a simple color legend for environment elements
        let legend = Row::new()
            .spacing(20)
            .push(
                Row::new()
                    .spacing(5)
                    .push(Container::new(Text::new(" "))
                        .width(Length::Fixed(16.0))
                        .height(Length::Fixed(16.0))
                        .style(iced::theme::Container::Custom(Box::new(
                            ColoredBox(self.get_cell_color(CellType::Food, None))
                        )))
                    )
                    .push(Text::new("Food").size(14))
            )
            .push(
                Row::new()
                    .spacing(5)
                    .push(Container::new(Text::new(" "))
                        .width(Length::Fixed(16.0))
                        .height(Length::Fixed(16.0))
                        .style(iced::theme::Container::Custom(Box::new(
                            ColoredBox(self.get_cell_color(CellType::Water, None))
                        )))
                    )
                    .push(Text::new("Water").size(14))
            )
            .push(
                Row::new()
                    .spacing(5)
                    .push(Container::new(Text::new(" "))
                        .width(Length::Fixed(16.0))
                        .height(Length::Fixed(16.0))
                        .style(iced::theme::Container::Custom(Box::new(
                            ColoredBox(self.get_cell_color(CellType::Obstacle, None))
                        )))
                    )
                    .push(Text::new("Obstacle").size(14))
            )
            .push(
                Row::new()
                    .spacing(5)
                    .push(Container::new(Text::new(" "))
                        .width(Length::Fixed(16.0))
                        .height(Length::Fixed(16.0))
                        .style(iced::theme::Container::Custom(Box::new(
                            ColoredBox(Color::from_rgb(1.0, 0.0, 0.0))
                        )))
                    )
                    .push(Text::new("Agent").size(14))
            );

        // Add a container with a border around the canvas for better visibility
        Container::new(
            Column::new()
                .spacing(10)
                .push(
                    Container::new(
                        Canvas::new(canvas)
                            .width(Length::Fixed((self.grid_size * self.cell_size as usize) as f32))
                            .height(Length::Fixed((self.grid_size * self.cell_size as usize) as f32))
                    )
                    .padding(5)
                    .style(iced::theme::Container::Custom(Box::new(GridOutline)))
                )
                .push(
                    Container::new(legend)
                        .width(Length::Fill)
                        .padding(5)
                        .style(iced::theme::Container::Custom(Box::new(LegendStyle)))
                )
        )
        .padding(10) // Increased padding
        .style(iced::theme::Container::Box)
        .into()
    }
    
    /// View the legend for cell types and strategies
    pub fn view_legend<'a>(&self, strategies: &'a StrategiesCollection) -> Element<'a, Message> {
        // Create a better organized cell types legend
        let cell_types = Column::new()
            .spacing(5)
            .push(Text::new("Environment Elements:").size(16))
            .push(
                Row::new()
                    .spacing(15)
                    .push(
                        Row::new()
                            .spacing(5)
                            .push(Container::new(Text::new(" "))
                                .width(Length::Fixed(16.0))
                                .height(Length::Fixed(16.0))
                                .style(iced::theme::Container::Custom(Box::new(
                                    ColoredBox(self.get_cell_color(CellType::Food, None))
                                )))
                            )
                            .push(Text::new("Food").size(14))
                    )
                    .push(
                        Row::new()
                            .spacing(5)
                            .push(Container::new(Text::new(" "))
                                .width(Length::Fixed(16.0))
                                .height(Length::Fixed(16.0))
                                .style(iced::theme::Container::Custom(Box::new(
                                    ColoredBox(self.get_cell_color(CellType::Water, None))
                                )))
                            )
                            .push(Text::new("Water").size(14))
                    )
                    .push(
                        Row::new()
                            .spacing(5)
                            .push(Container::new(Text::new(" "))
                                .width(Length::Fixed(16.0))
                                .height(Length::Fixed(16.0))
                                .style(iced::theme::Container::Custom(Box::new(
                                    ColoredBox(self.get_cell_color(CellType::Obstacle, None))
                                )))
                            )
                            .push(Text::new("Obstacle").size(14))
                    )
                    .push(
                        Row::new()
                            .spacing(5)
                            .push(Container::new(Text::new(" "))
                                .width(Length::Fixed(16.0))
                                .height(Length::Fixed(16.0))
                                .style(iced::theme::Container::Custom(Box::new(
                                    ColoredBox(Color::from_rgb(1.0, 0.0, 0.0))
                                )))
                            )
                            .push(Text::new("Generic Agent").size(14))
                    )
            );
        
        // Create an improved strategy legend with a descriptive header
        let mut strategy_legend = Column::new()
            .spacing(10)
            .push(Text::new("Agent Strategies").size(18))
            .push(Text::new("Each strategy has a unique color to identify agents in the simulation:").size(14));
        
        // Create a grid layout for strategies
        let strategies_per_row = 3; // Allow more strategies per row for better display
        
        let mut strategy_grid = Column::new().spacing(15);
        let mut current_row = Row::new().spacing(30);
        let mut items_in_row = 0;
        
        for (name, params) in &strategies.strategies {
            let strategy_color = params.generate_color();
            
            let strategy_item = Row::new()
                .spacing(10)
                .push(
                    Container::new(Text::new(" "))
                        .width(Length::Fixed(24.0))
                        .height(Length::Fixed(24.0))
                        .style(iced::theme::Container::Custom(Box::new(
                            ColoredBox(strategy_color)
                        )))
                )
                .push(
                    Text::new(name).size(16)
                );
            
            current_row = current_row.push(strategy_item);
            items_in_row += 1;
            
            // Start a new row when we've filled the current one
            if items_in_row >= strategies_per_row {
                strategy_grid = strategy_grid.push(current_row);
                current_row = Row::new().spacing(30);
                items_in_row = 0;
            }
        }
        
        // Add the last row if it has any items
        if items_in_row > 0 {
            strategy_grid = strategy_grid.push(current_row);
        }
        
        // Add the grid to the strategy legend
        strategy_legend = strategy_legend.push(strategy_grid);
        
        // Create an improved tracked agents section
        let mut tracked_section = Column::new()
            .spacing(10)
            .push(Text::new("Tracked Agents").size(18))
            .push(Text::new("Select agents to track their movement through the simulation:").size(14));
        
        if self.tracked_agents.is_empty() {
            tracked_section = tracked_section.push(
                Container::new(
                    Text::new("No agents are currently being tracked. Click on an agent to track it.")
                        .size(14)
                )
                .padding(10)
            );
        } else {
            let mut tracked_row = Row::new().spacing(20);
            
            for agent in &self.tracked_agents {
                let is_selected = self.selected_agent.as_ref()
                    .map_or(false, |selected| selected.id == agent.id);
                
                let agent_display = Row::new()
                    .spacing(10)
                    .push(Container::new(Text::new(" "))
                        .width(Length::Fixed(24.0))
                        .height(Length::Fixed(24.0))
                        .style(iced::theme::Container::Custom(Box::new(
                            ColoredBox(agent.color)
                        )))
                    )
                    .push(
                        Column::new()
                            .spacing(2)
                            .push(Text::new(agent.strategy_name.clone()).size(16))
                            .push(Text::new(format!("Agent #{}", &agent.id[..6])).size(12))
                    );
                
                let agent_button = Button::new(agent_display)
                    .on_press(Message::TrackAgent(agent.id.clone()))
                    .style(if is_selected {
                        iced::theme::Button::Primary
                    } else {
                        iced::theme::Button::Secondary
                    });
                
                tracked_row = tracked_row.push(agent_button);
            }
            
            // Add a clear button
            tracked_row = tracked_row.push(
                Button::new(
                    Container::new(
                        Text::new("Clear All Tracked").size(16)
                    )
                    .padding(8)
                )
                .on_press(Message::ClearTrackedAgents)
                .style(iced::theme::Button::Destructive)
                .padding(5)
            );
            
            tracked_section = tracked_section.push(tracked_row);
        }
        
        // Create a better day/night indicator
        let day_night_indicator = Column::new()
            .spacing(5)
            .push(Text::new("Simulation Time:").size(16))
            .push(
                Row::new()
                    .spacing(5)
                    .push(
                        Container::new(Text::new(""))
                            .width(Length::Fixed(16.0))
                            .height(Length::Fixed(16.0))
                            .style(iced::theme::Container::Custom(Box::new(
                                ColoredBox(match self.time_of_day {
                                    TimeOfDay::Day => Color::from_rgb(1.0, 0.8, 0.0),  // Yellow for day
                                    TimeOfDay::Night => Color::from_rgb(0.2, 0.2, 0.5), // Dark blue for night
                                })
                            )))
                    )
                    .push(Text::new(match self.time_of_day {
                        TimeOfDay::Day => "Day ☀️",
                        TimeOfDay::Night => "Night 🌙",
                    }).size(14))
            );
            
        // Create a better legend section for the environment and time
        let environment_time_row = Row::new()
            .spacing(30)
            .push(cell_types)
            .push(day_night_indicator);
            
        // Create a separate container for strategy legend to make it more prominent
        let strategy_container = Container::new(
            strategy_legend
        )
        .width(Length::Fill)
        .padding(10)
        .style(iced::theme::Container::Custom(Box::new(StrategyLegendStyle)));
        
        // Create a legend container to hold all legend sections
        let legend_container = Container::new(
            Column::new()
                .spacing(15)
                .push(environment_time_row)
                .push(strategy_container)
                .push(tracked_section)
        )
        .padding(10)
        .style(iced::theme::Container::Box);
        
        legend_container.into()
    }
}

/// Canvas for rendering the grid
struct GridCanvas<'a> {
    grid_size: usize,
    cell_size: u16,
    grid_state: &'a Vec<Vec<CellState>>,
    show_sensing_radius: bool,
    show_agent_paths: bool,
    selected_agent: Option<&'a Agent>,
    tracked_agents: &'a [Agent],
    time_of_day: TimeOfDay,
    cell_colors: Vec<Vec<Color>>,
    cell_opacities: Vec<Vec<f32>>,
}

impl<'a> canvas::Program<Message> for GridCanvas<'a> {
    type State = ();
    
    fn draw(
        &self,
        _state: &Self::State,
        renderer: &iced::Renderer,
        _theme: &Theme,
        bounds: Rectangle,
        _cursor: iced::mouse::Cursor,
    ) -> Vec<canvas::Geometry> {
        let cell_size = self.cell_size as f32;
        let mut frame = Frame::new(renderer, bounds.size());
        
        // Draw grid cells
        for y in 0..self.grid_size {
            for x in 0..self.grid_size {
                let cell = &self.grid_state[y][x];
                let is_selected = self.selected_agent.as_ref()
                    .map_or(false, |agent| agent.x == x && agent.y == y);
                
                let is_tracked_path = self.show_agent_paths && 
                    self.tracked_agents.iter().any(|agent| 
                        agent.movement_history.iter().any(|pos| pos.x == x && pos.y == y)
                    );
                
                let color = self.cell_colors[y][x];
                let opacity = self.cell_opacities[y][x];
                
                // Draw the cell
                let cell_path = Path::rectangle(
                    Point::new(x as f32 * cell_size, y as f32 * cell_size),
                    Size::new(cell_size, cell_size),
                );
                
                let mut color_with_alpha = color;
                color_with_alpha.a = opacity;
                frame.fill(&cell_path, color_with_alpha);
                
                // Always draw a grid line around each cell for better visibility
                let border = Path::rectangle(
                    Point::new(x as f32 * cell_size, y as f32 * cell_size),
                    Size::new(cell_size, cell_size),
                );
                
                frame.stroke(&border, Stroke {
                    style: iced::widget::canvas::Style::Solid(Color::from_rgba(0.0, 0.0, 0.0, 0.2)),
                    width: 0.5,
                    line_cap: LineCap::Round,
                    line_join: LineJoin::Round,
                    line_dash: LineDash::default(),
                });
                
                // Draw border for selected or tracked cells
                if is_selected {
                    let border = Path::rectangle(
                        Point::new(x as f32 * cell_size, y as f32 * cell_size),
                        Size::new(cell_size, cell_size),
                    );
                    
                    frame.stroke(&border, Stroke {
                        style: iced::widget::canvas::Style::Solid(Color::from_rgb(1.0, 0.0, 0.0)),
                        width: 2.0,
                        line_cap: LineCap::Round,
                        line_join: LineJoin::Round,
                        line_dash: LineDash::default(),
                    });
                } else if is_tracked_path {
                    let border = Path::rectangle(
                        Point::new(x as f32 * cell_size, y as f32 * cell_size),
                        Size::new(cell_size, cell_size),
                    );
                    
                    frame.stroke(&border, Stroke {
                        style: iced::widget::canvas::Style::Solid(Color::from_rgb(1.0, 0.8, 0.0)),
                        width: 1.0,
                        line_cap: LineCap::Round,
                        line_join: LineJoin::Round,
                        line_dash: LineDash::default(),
                    });
                }
                
                // Draw sensing radius for agents if enabled
                if self.show_sensing_radius && cell.cell_type == CellType::Agent {
                    let radius = SENSING_RADIUS as f32 * cell_size;
                    let center_x = (x as f32 + 0.5) * cell_size;
                    let center_y = (y as f32 + 0.5) * cell_size;
                    
                    let sensing_radius = Path::circle(
                        Point::new(center_x, center_y),
                        radius,
                    );
                    
                    frame.stroke(&sensing_radius, Stroke {
                        style: iced::widget::canvas::Style::Solid(Color::from_rgba(1.0, 0.0, 0.0, 0.3)),
                        width: 1.0,
                        line_cap: LineCap::Round,
                        line_join: LineJoin::Round,
                        line_dash: LineDash::default(),
                    });
                }
            }
        }
        
        // Draw day/night indicator
        let indicator_size = 30.0;
        let indicator_x = bounds.width - indicator_size - 10.0;
        let indicator_y = 10.0;
        
        let indicator = Path::circle(
            Point::new(indicator_x + indicator_size / 2.0, indicator_y + indicator_size / 2.0),
            indicator_size / 2.0,
        );
        
        let indicator_color = match self.time_of_day {
            TimeOfDay::Day => Color::from_rgb(1.0, 0.8, 0.0),  // Yellow for day
            TimeOfDay::Night => Color::from_rgb(0.2, 0.2, 0.5), // Dark blue for night
        };
        
        frame.fill(&indicator, indicator_color);
        
        vec![frame.into_geometry()]
    }
    
    fn mouse_interaction(
        &self,
        _state: &Self::State,
        bounds: Rectangle,
        cursor: iced::mouse::Cursor,
    ) -> mouse::Interaction {
        if cursor.is_over(bounds) {
            mouse::Interaction::Pointer
        } else {
            mouse::Interaction::default()
        }
    }
    
    fn update(
        &self,
        _state: &mut Self::State,
        event: canvas::Event,
        bounds: Rectangle,
        cursor: iced::mouse::Cursor,
    ) -> (canvas::event::Status, Option<Message>) {
        match event {
            canvas::Event::Mouse(mouse::Event::ButtonPressed(mouse::Button::Left)) => {
                if let Some(position) = cursor.position_in(bounds) {
                    let cell_size = self.cell_size as f32;
                    let x = (position.x / cell_size) as usize;
                    let y = (position.y / cell_size) as usize;
                    
                    if x < self.grid_size && y < self.grid_size {
                        return (
                            canvas::event::Status::Captured,
                            Some(Message::CellClicked(x, y)),
                        );
                    }
                }
            }
            _ => {}
        }
        
        (canvas::event::Status::Ignored, None)
    }
}

/// Style for colored boxes in the legend
struct ColoredBox(Color);

impl iced::widget::container::StyleSheet for ColoredBox {
    type Style = iced::Theme;
    
    fn appearance(&self, _style: &Self::Style) -> iced::widget::container::Appearance {
        iced::widget::container::Appearance {
            background: Some(iced::Background::Color(self.0)),
            border_radius: 2.0.into(),
            border_width: 1.0,
            border_color: Color::BLACK,
            ..Default::default()
        }
    }
}

// GridOutline style for the simulation grid
struct GridOutline;

impl iced::widget::container::StyleSheet for GridOutline {
    type Style = iced::Theme;
    
    fn appearance(&self, _style: &Self::Style) -> iced::widget::container::Appearance {
        iced::widget::container::Appearance {
            background: Some(iced::Background::Color(Color::from_rgb(0.92, 0.92, 0.92))),
            border_radius: 2.0.into(),
            border_width: 2.0,
            border_color: Color::from_rgb(0.3, 0.3, 0.3),
            ..Default::default()
        }
    }
}

// Style for the strategy legend container to make it stand out
struct StrategyLegendStyle;

impl iced::widget::container::StyleSheet for StrategyLegendStyle {
    type Style = iced::Theme;
    
    fn appearance(&self, _style: &Self::Style) -> iced::widget::container::Appearance {
        iced::widget::container::Appearance {
            background: Some(iced::Background::Color(Color::from_rgb(0.95, 0.95, 1.0))),
            border_radius: 5.0.into(),
            border_width: 2.0,
            border_color: Color::from_rgb(0.4, 0.4, 0.8),
            text_color: Some(Color::BLACK),
            ..Default::default()
        }
    }
}

// Style for the simple legend display under the grid
struct LegendStyle;

impl iced::widget::container::StyleSheet for LegendStyle {
    type Style = iced::Theme;
    
    fn appearance(&self, _style: &Self::Style) -> iced::widget::container::Appearance {
        iced::widget::container::Appearance {
            background: Some(iced::Background::Color(Color::from_rgb(0.97, 0.97, 0.97))),
            border_radius: 4.0.into(),
            border_width: 1.0,
            border_color: Color::from_rgb(0.7, 0.7, 0.7),
            text_color: Some(Color::BLACK),
            ..Default::default()
        }
    }
}