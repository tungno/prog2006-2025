use iced::{
    widget::{Column, Container, Row, Text, Button, Checkbox, Slider, Space, container},
    Element, Length, Color, Background, Theme,
};

use crate::ui::{Message, HeatmapType};
use crate::constants::{FOOD_REGENERATION_RATE, WATER_REGENERATION_RATE};
use crate::simulation::Simulation;

/// Controls panel for the simulation
pub struct ControlsPanel {
    pub food_regen_rate: f32,
    pub water_regen_rate: f32,
    pub show_sensing_radius: bool,
    pub show_agent_paths: bool,
    pub show_heatmap: bool,
    pub heatmap_type: HeatmapType,
    alive_agents: usize,
    average_health: f32,
    average_age: f32,
    average_lifespan: f32,
    generation: usize,
    food_count: usize,
    water_count: usize,
    strategy_performance: String,
    pub running: bool,
    pub tick_speed: u64,
}

impl ControlsPanel {
    /// Create a new controls panel
    pub fn new() -> Self {
        Self {
            food_regen_rate: FOOD_REGENERATION_RATE,
            water_regen_rate: WATER_REGENERATION_RATE,
            show_sensing_radius: false,
            show_agent_paths: false,
            show_heatmap: false,
            heatmap_type: HeatmapType::Food,
            alive_agents: 0,
            average_health: 0.0,
            average_age: 0.0,
            average_lifespan: 0.0,
            generation: 1,
            food_count: 0,
            water_count: 0,
            strategy_performance: String::new(),
            running: false,
            tick_speed: 200,
        }
    }
    
    /// Update the control panel state from the simulation
    pub fn update_from_simulation(&mut self, simulation: &Simulation) {
        self.food_regen_rate = simulation.environment.resource_manager.food_regeneration_rate();
        self.water_regen_rate = simulation.environment.resource_manager.water_regeneration_rate();
        
        self.alive_agents = simulation.alive_agents_count();
        self.average_health = simulation.average_health();
        self.average_age = simulation.average_age();
        self.average_lifespan = simulation.statistics.evolution_metrics.average_lifespan;
        self.generation = simulation.statistics.evolution_metrics.generation;
        
        self.food_count = simulation.environment.food_count();
        self.water_count = simulation.environment.water_count();
        
        // Format strategy performance
        let mut performance = String::new();
        for (strategy, data) in &simulation.statistics.evolution_metrics.strategy_performance {
            performance.push_str(&format!(
                "{}: {} agents, avg age: {:.1}\n",
                strategy, data.count, data.avg_age
            ));
        }
        self.strategy_performance = performance;
        
        self.running = simulation.is_running();
    }
    
    /// Set the simulation running state
    pub fn set_running(&mut self, running: bool) {
        self.running = running;
    }
    
    /// Set the tick speed
    pub fn set_tick_speed(&mut self, speed: u64) {
        self.tick_speed = speed;
    }
    
    /// Set the show sensing radius flag
    pub fn set_show_sensing_radius(&mut self, show: bool) {
        self.show_sensing_radius = show;
    }
    
    /// Set the show agent paths flag
    pub fn set_show_agent_paths(&mut self, show: bool) {
        self.show_agent_paths = show;
    }
    
    /// Set the show heatmap flag
    pub fn set_show_heatmap(&mut self, show: bool) {
        self.show_heatmap = show;
    }
    
    /// Set the heatmap type
    pub fn set_heatmap_type(&mut self, heatmap_type: HeatmapType) {
        self.heatmap_type = heatmap_type;
    }
    
    /// View the controls panel
    pub fn view(&self) -> Element<Message> {
        let controls = Column::new()
            .spacing(5)
            .push(Text::new("Controls").size(16))
            .push(
                Row::new()
                    .spacing(5)
                    .push(
                        Button::new(Text::new(if self.running { "Pause" } else { "Start" }))
                            .on_press(Message::ToggleRunning)
                            .style(if self.running {
                                iced::theme::Button::Destructive
                            } else {
                                iced::theme::Button::Primary
                            })
                            .width(Length::Fill)
                    )
                    .push(
                        Button::new(Text::new("Reset"))
                            .on_press(Message::Reset)
                            .style(iced::theme::Button::Secondary)
                            .width(Length::Fill)
                    )
            )
            .push(
                Column::new()
                    .spacing(5)
                    .push(Text::new(format!("Simulation Speed: {}ms", self.tick_speed)).size(12))
                    .push(
                        Slider::new(
                            50..=500,
                            self.tick_speed as i32,
                            |v| Message::SetTickSpeed(v as u64)
                        )
                        .step(50)
                    )
            )
            .push(Space::with_height(Length::Fixed(10.0)))
            .push(
                Column::new()
                    .spacing(5)
                    .push(Text::new(format!("Food Regeneration: {:.0}%", self.food_regen_rate * 100.0)).size(12))
                    .push(
                        Slider::new(
                            1..=20,
                            (self.food_regen_rate * 100.0) as i32,
                            |v| Message::SetFoodRegenerationRate(v as f32 / 100.0)
                        )
                        .step(1)
                    )
            )
            .push(
                Column::new()
                    .spacing(5)
                    .push(Text::new(format!("Water Regeneration: {:.0}%", self.water_regen_rate * 100.0)).size(12))
                    .push(
                        Slider::new(
                            1..=20,
                            (self.water_regen_rate * 100.0) as i32,
                            |v| Message::SetWaterRegenerationRate(v as f32 / 100.0)
                        )
                        .step(1)
                    )
            )
            .push(Space::with_height(Length::Fixed(10.0)))
            .push(
                Column::new()
                    .spacing(5)
                    .push(
                        Checkbox::new(
                            "Show Sensing Radius",
                            self.show_sensing_radius,
                            |_| Message::ToggleShowSensingRadius
                        )
                    )
                    .push(
                        Checkbox::new(
                            "Show Agent Paths",
                            self.show_agent_paths,
                            |_| Message::ToggleShowAgentPaths
                        )
                    )
                    .push(
                        Checkbox::new(
                            "Show Heatmap",
                            self.show_heatmap,
                            |_| Message::ToggleShowHeatmap
                        )
                    )
            );
            
        let heatmap_controls = if self.show_heatmap {
            Column::new()
                .spacing(5)
                .push(Space::with_height(Length::Fixed(5.0)))
                .push(Text::new("Heatmap Type:").size(12))
                .push(
                    Row::new()
                        .spacing(10)
                        .push(
                            Radio::new(
                                "Food Activity",
                                HeatmapType::Food,
                                Some(self.heatmap_type),
                                Message::SetHeatmapType
                            )
                        )
                        .push(
                            Radio::new(
                                "Water Proximity",
                                HeatmapType::Water,
                                Some(self.heatmap_type),
                                Message::SetHeatmapType
                            )
                        )
                )
        } else {
            Column::new()
        };
            
        let statistics = Column::new()
            .spacing(5)
            .push(Space::with_height(Length::Fixed(20.0)))
            .push(Text::new("Statistics").size(16))
            .push(Text::new(format!("Generation: {}", self.generation)).size(12))
            .push(Text::new(format!("Alive Agents: {}", self.alive_agents)).size(12))
            .push(Text::new(format!("Average Health: {:.1}", self.average_health)).size(12))
            .push(Text::new(format!("Average Age: {:.1}", self.average_age)).size(12))
            .push(Text::new(format!("Average Lifespan: {:.1}", self.average_lifespan)).size(12))
            .push(Text::new(format!("Resources: Food ({}), Water ({})", self.food_count, self.water_count)).size(12))
            .push(Space::with_height(Length::Fixed(10.0)))
            .push(Text::new("Strategy Performance:").size(14))
            .push(
                Container::new(Text::new(&self.strategy_performance).size(12))
                    .width(Length::Fill)
                    .height(Length::Fixed(100.0))
                    .style(iced::theme::Container::Box)
            );
            
        let evolution = Column::new()
            .spacing(10)
            .push(Space::with_height(Length::Fixed(20.0)))
            .push(
                Button::new(Text::new("Trigger Manual Evolution"))
                    .on_press(Message::TriggerEvolution)
                    .style(if self.running {
                        iced::theme::Button::Secondary
                    } else {
                        iced::theme::Button::Primary
                    })
                    .width(Length::Fill)
            );
        
        Container::new(
            Column::new()
                .spacing(10)
                .push(controls)
                .push(heatmap_controls)
                .push(statistics)
                .push(evolution)
                .width(Length::Fixed(250.0))
        )
        .style(iced::theme::Container::Box)
        .padding(10)
        .into()
    }
}

/// A simple radio button widget
struct Radio<T: Copy + Eq> {
    label: String,
    value: T,
    selected: Option<T>,
    on_click: fn(T) -> Message,
}

impl<T: Copy + Eq> Radio<T> {
    fn new(
        label: impl Into<String>,
        value: T,
        selected: Option<T>,
        on_click: fn(T) -> Message,
    ) -> Self {
        Self {
            label: label.into(),
            value,
            selected,
            on_click,
        }
    }
}

impl<T: Copy + Eq> From<Radio<T>> for Element<'_, Message> {
    fn from(radio: Radio<T>) -> Self {
        let is_selected = radio.selected == Some(radio.value);
        
        let dot = Container::new(Space::new(Length::Fixed(8.0), Length::Fixed(8.0)))
            .width(Length::Fixed(16.0))
            .height(Length::Fixed(16.0))
            .style(if is_selected {
                iced::theme::Container::Custom(Box::new(RadioSelected))
            } else {
                iced::theme::Container::Custom(Box::new(RadioDeselected))
            });
            
        Button::new(
            Row::new()
                .spacing(10)
                .push(dot)
                .push(Text::new(radio.label))
        )
        .on_press((radio.on_click)(radio.value))
        .style(iced::theme::Button::Text)
        .into()
    }
}

/// Style for selected radio buttons
struct RadioSelected;

impl container::StyleSheet for RadioSelected {
    type Style = Theme;
    
    fn appearance(&self, _style: &Self::Style) -> container::Appearance {
        container::Appearance {
            background: Some(Background::Color(Color::from_rgb(0.3, 0.65, 1.0))),
            border_radius: 8.0.into(),
            border_width: 1.0,
            border_color: Color::BLACK,
            ..Default::default()
        }
    }
}

/// Style for deselected radio buttons
struct RadioDeselected;

impl container::StyleSheet for RadioDeselected {
    type Style = Theme;
    
    fn appearance(&self, _style: &Self::Style) -> container::Appearance {
        container::Appearance {
            background: Some(Background::Color(Color::TRANSPARENT)),
            border_radius: 8.0.into(),
            border_width: 1.0,
            border_color: Color::BLACK,
            ..Default::default()
        }
    }
}