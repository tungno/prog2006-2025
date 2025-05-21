use iced::{
    widget::{Column, Container, Row, Text, Button, Slider, TextInput, Space, Scrollable, container},
    Element, Length, Color, Background, Theme,
};
use std::collections::HashMap;
use crate::ui::{Message};
use crate::heuristics::{StrategiesCollection, StrategyParameters};

/// The UI component for editing heuristics
pub struct HeuristicEditor {
    strategies: HashMap<String, StrategyParameters>,
    editing_strategy: Option<EditingStrategy>,
    new_strategy_name: String,
}

/// Currently editing strategy state
#[derive(Debug, Clone)]
struct EditingStrategy {
    name: String,
    params: StrategyParameters,
}

impl HeuristicEditor {
    /// Create a new heuristic editor
    pub fn new(strategies: &StrategiesCollection) -> Self {
        Self {
            strategies: strategies.strategies.clone(),
            editing_strategy: None,
            new_strategy_name: String::new(),
        }
    }
    
    /// Start editing a strategy
    pub fn start_editing(&mut self, name: &str, params: Option<StrategyParameters>) {
        let params = params.unwrap_or_else(|| 
            self.strategies.get(name)
                .cloned()
                .unwrap_or_else(|| StrategyParameters::new(name))
        );
        
        self.editing_strategy = Some(EditingStrategy {
            name: name.to_string(),
            params,
        });
    }
    
    /// Cancel editing a strategy
    pub fn cancel_editing(&mut self) {
        self.editing_strategy = None;
    }
    
    /// View the heuristic editor
    pub fn view(&self) -> Element<Message> {
        let content = if let Some(editing) = &self.editing_strategy {
            self.view_strategy_editor(editing)
        } else {
            self.view_strategies_list()
        };
        
        Container::new(content)
            .width(Length::Fill)
            .height(Length::Fill)
            .center_x()
            .into()
    }
    
    /// View the list of available strategies
    fn view_strategies_list(&self) -> Element<Message> {
        let title = Text::new("Heuristics Manager").size(24);
        
        let export_import = Row::new()
            .spacing(10)
            .push(
                Button::new(Text::new("Export Strategies"))
                    .on_press(Message::ExportStrategies)
                    .style(iced::theme::Button::Primary)
            )
            .push(
                Button::new(Text::new("Import Strategies"))
                    .on_press(Message::ImportStrategies(self.strategies.clone()))
                    .style(iced::theme::Button::Secondary)
            );
        
        let new_strategy = Column::new()
            .spacing(10)
            .push(Text::new("Create New Strategy").size(18))
            .push(
                Row::new()
                    .spacing(10)
                    .push(
                        TextInput::new(
                            "New strategy name",
                            &self.new_strategy_name
                        )
                        .on_input(|s| Message::CreateStrategy(s))
                        .padding(8)
                        .width(Length::Fill)
                    )
                    .push(
                        Button::new(Text::new("Create"))
                            .on_press(Message::CreateStrategy(self.new_strategy_name.clone()))
                            .style(iced::theme::Button::Primary)
                    )
            );
        
        let strategies_title = Text::new("Available Strategies").size(18);
        
        // Create a grid of strategy cards
        let strategies_grid = if self.strategies.is_empty() {
            Column::new().push(Text::new("No strategies available").size(16))
        } else {
            let mut rows = Vec::new();
            let mut current_row = Row::new().spacing(20);
            let mut count = 0;
            
            for (name, strategy) in &self.strategies {
                if count > 0 && count % 3 == 0 {
                    rows.push(current_row);
                    current_row = Row::new().spacing(20);
                }
                
                let card = self.create_strategy_card(name, strategy);
                current_row = current_row.push(card);
                count += 1;
            }
            
            if count % 3 != 0 {
                rows.push(current_row);
            }
            
            let mut column = Column::new().spacing(20);
            for row in rows {
                column = column.push(row);
            }
            
            column
        };
        
        Column::new()
            .spacing(20)
            .push(title)
            .push(export_import)
            .push(new_strategy)
            .push(Space::with_height(Length::Fixed(20.0)))
            .push(strategies_title)
            .push(
                Scrollable::new(strategies_grid)
                    .height(Length::Fill)
            )
            .padding(20)
            .width(Length::Fill)
            .into()
    }
    
    /// Create a card for a strategy
    fn create_strategy_card<'a>(&self, name: &'a str, strategy: &'a StrategyParameters) -> Element<'a, Message> {
        let color = strategy.generate_color();
        
        let header = Row::new()
            .spacing(10)
            .push(
                Container::new(Space::new(Length::Fixed(16.0), Length::Fixed(16.0)))
                    .style(iced::theme::Container::Custom(Box::new(ColorBox(color))))
            )
            .push(Text::new(name).size(16));
        
        let values = Column::new()
            .spacing(5)
            .push(Text::new(format!("Food Weight: {:.2}", strategy.food_weight)).size(12))
            .push(Text::new(format!("Water Weight: {:.2}", strategy.water_weight)).size(12))
            .push(Text::new(format!("Energy Conservation: {:.2}", strategy.energy_conservation)).size(12))
            .push(Text::new(format!("Exploration: {:.2}", strategy.exploration_tendency)).size(12))
            .push(Text::new(format!("Night Behavior: {:.2}", strategy.night_behavior)).size(12));
        
        let buttons = Row::new()
            .spacing(10)
            .push(
                Button::new(Text::new("Edit").size(12))
                    .on_press(Message::EditStrategy(name.to_string()))
                    .style(iced::theme::Button::Primary)
                    .width(Length::Fill)
            )
            .push(
                Button::new(Text::new("Delete").size(12))
                    .on_press(Message::DeleteStrategy(name.to_string()))
                    .style(iced::theme::Button::Destructive)
                    .width(Length::Fill)
            );
        
        Container::new(
            Column::new()
                .spacing(10)
                .push(header)
                .push(values)
                .push(buttons)
                .width(Length::Fixed(250.0))
                .padding(10)
        )
        .style(iced::theme::Container::Box)
        .into()
    }
    
    /// View the strategy editor
    fn view_strategy_editor<'a>(&self, editing: &'a EditingStrategy) -> Element<'a, Message> {
        let title = Text::new(format!("Edit Strategy: {}", editing.name)).size(24);
        
        let params = &editing.params;
        
        let sliders = Column::new()
            .spacing(15)
            .push(
                Column::new()
                    .spacing(5)
                    .push(Text::new(format!("Food Weight: {:.2}", params.food_weight)).size(14))
                    .push(
                        Slider::new(
                            10..=90,
                            (params.food_weight * 100.0) as i32,
                            |value| {
                                let mut new_params = params.clone();
                                new_params.food_weight = value as f32 / 100.0;
                                Message::UpdateStrategy(new_params)
                            }
                        )
                        .step(5)
                    )
                    .push(
                        Row::new()
                            .push(Text::new("0.1").size(10))
                            .push(Space::with_width(Length::Fill))
                            .push(Text::new("0.9").size(10))
                    )
            )
            .push(
                Column::new()
                    .spacing(5)
                    .push(Text::new(format!("Water Weight: {:.2}", params.water_weight)).size(14))
                    .push(
                        Slider::new(
                            10..=90,
                            (params.water_weight * 100.0) as i32,
                            |value| {
                                let mut new_params = params.clone();
                                new_params.water_weight = value as f32 / 100.0;
                                Message::UpdateStrategy(new_params)
                            }
                        )
                        .step(5)
                    )
                    .push(
                        Row::new()
                            .push(Text::new("0.1").size(10))
                            .push(Space::with_width(Length::Fill))
                            .push(Text::new("0.9").size(10))
                    )
            )
            .push(
                Column::new()
                    .spacing(5)
                    .push(Text::new(format!("Energy Conservation: {:.2}", params.energy_conservation)).size(14))
                    .push(
                        Slider::new(
                            10..=90,
                            (params.energy_conservation * 100.0) as i32,
                            |value| {
                                let mut new_params = params.clone();
                                new_params.energy_conservation = value as f32 / 100.0;
                                Message::UpdateStrategy(new_params)
                            }
                        )
                        .step(5)
                    )
                    .push(
                        Row::new()
                            .push(Text::new("0.1").size(10))
                            .push(Space::with_width(Length::Fill))
                            .push(Text::new("0.9").size(10))
                    )
            )
            .push(
                Column::new()
                    .spacing(5)
                    .push(Text::new(format!("Exploration Tendency: {:.2}", params.exploration_tendency)).size(14))
                    .push(
                        Slider::new(
                            10..=90,
                            (params.exploration_tendency * 100.0) as i32,
                            |value| {
                                let mut new_params = params.clone();
                                new_params.exploration_tendency = value as f32 / 100.0;
                                Message::UpdateStrategy(new_params)
                            }
                        )
                        .step(5)
                    )
                    .push(
                        Row::new()
                            .push(Text::new("0.1").size(10))
                            .push(Space::with_width(Length::Fill))
                            .push(Text::new("0.9").size(10))
                    )
            )
            .push(
                Column::new()
                    .spacing(5)
                    .push(Text::new(format!("Night Behavior: {:.2}", params.night_behavior)).size(14))
                    .push(
                        Slider::new(
                            10..=90,
                            (params.night_behavior * 100.0) as i32,
                            |value| {
                                let mut new_params = params.clone();
                                new_params.night_behavior = value as f32 / 100.0;
                                Message::UpdateStrategy(new_params)
                            }
                        )
                        .step(5)
                    )
                    .push(
                        Row::new()
                            .push(Text::new("0.1").size(10))
                            .push(Space::with_width(Length::Fill))
                            .push(Text::new("0.9").size(10))
                    )
            );
        
        let buttons = Row::new()
            .spacing(10)
            .push(
                Button::new(Text::new("Save"))
                    .on_press(Message::UpdateStrategy(params.clone()))
                    .style(iced::theme::Button::Primary)
            )
            .push(
                Button::new(Text::new("Cancel"))
                    .on_press(Message::CancelStrategyEdit)
                    .style(iced::theme::Button::Secondary)
            );
        
        Column::new()
            .spacing(20)
            .push(title)
            .push(
                Container::new(sliders)
                    .width(Length::Fixed(400.0))
                    .style(iced::theme::Container::Box)
                    .padding(20)
            )
            .push(buttons)
            .padding(20)
            .width(Length::Fill)
            .into()
    }
}

/// Custom colored box style
struct ColorBox(Color);

impl container::StyleSheet for ColorBox {
    type Style = Theme;
    
    fn appearance(&self, _style: &Self::Style) -> container::Appearance {
        container::Appearance {
            background: Some(Background::Color(self.0)),
            border_radius: 4.0.into(),
            border_width: 1.0,
            border_color: Color::BLACK,
            ..Default::default()
        }
    }
}