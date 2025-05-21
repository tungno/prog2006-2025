use iced::{
    widget::{Column, Container, Row, Text, Button, PickList, Scrollable, Rule, Space, Canvas},
    Element, Length,
};

use crate::ui::Message;
use crate::simulation::Simulation;
use crate::statistics::{StatisticType, Chart, create_line_chart, create_strategy_chart, HistoryPoint};
use crate::evolution::StrategyPerformance;
use std::collections::HashMap;

/// The UI component for displaying statistics
pub struct StatisticsView {
    selected_stat_type: StatisticType,
}

impl StatisticsView {
    /// Create a new statistics view
    pub fn new() -> Self {
        Self {
            selected_stat_type: StatisticType::Population,
        }
    }
    
    /// Update the view from the current simulation state
    pub fn update_from_simulation(&mut self, _simulation: &Simulation) {
        // No state to update, charts will be generated directly from simulation data
    }
    
    /// Select a different statistic type
    pub fn select_statistic_type(&mut self, stat_type: StatisticType) {
        self.selected_stat_type = stat_type;
    }
    
    /// View the statistics
    pub fn view<'a>(&self, simulation: &'a Simulation) -> Element<'a, Message> {
        let title = Text::new("Detailed Statistics").size(24);
        
        // Create a dropdown for selecting which statistic to view
        let stat_types: Vec<StatisticType> = StatisticType::all();
        // Clone the stat types to ensure they live long enough
        let owned_stat_types = stat_types.clone();
        let stat_picker = Row::new()
            .spacing(10)
            .push(Text::new("Select Statistic: ").size(16))
            .push(
                PickList::new(
                    owned_stat_types,
                    Some(self.selected_stat_type),
                    Message::SelectStatisticType
                )
                .width(Length::Fixed(200.0))
            );
            
        // Convert simulation data to chart data points
        let history_points = self.get_history_points_for_statistic(simulation, self.selected_stat_type);
        let resource_data = self.get_resource_history_points(simulation);
        let strategy_data = self.get_strategy_distribution(simulation);
        
        // We need to own these chart objects so they live for the entire function
        #[allow(unused_variables)]
        let _all_elements: Vec<Element<'a, Message>> = Vec::new();
        
        // Create the main chart (owned by the function)
        let main_chart = Canvas::new(create_line_chart(
            &history_points,
            self.selected_stat_type,
            600.0, 
            300.0
        ))
        .width(Length::Fill)
        .height(Length::Fixed(300.0));
        
        // Create resource chart (owned by the function)
        let resource_chart = Canvas::new(create_line_chart(
            &resource_data,
            StatisticType::FoodCount,
            600.0,
            200.0
        ))
        .width(Length::Fill)
        .height(Length::Fixed(200.0));
        
        // Create strategy chart (owned by the function)
        let strategy_chart = Canvas::new(create_strategy_chart(
            &strategy_data,
            600.0,
            200.0
        ))
        .width(Length::Fill)
        .height(Length::Fixed(200.0));
        
        // Strategy performance table
        let performance_table = self.create_strategy_performance_table(simulation);
        
        // Assemble the view
        Column::new()
            .spacing(20)
            .push(title)
            .push(stat_picker)
            .push(
                Container::new(main_chart)
                    .width(Length::Fill)
                    .center_x()
                    .style(iced::theme::Container::Box)
                    .padding(10)
            )
            .push(
                Row::new()
                    .spacing(20)
                    .push(
                        Container::new(
                            Column::new()
                                .spacing(10)
                                .push(Text::new("Resource Levels").size(16))
                                .push(resource_chart)
                        )
                        .width(Length::Fill)
                        .style(iced::theme::Container::Box)
                        .padding(10)
                    )
                    .push(
                        Container::new(
                            Column::new()
                                .spacing(10)
                                .push(Text::new("Strategy Distribution").size(16))
                                .push(strategy_chart)
                        )
                        .width(Length::Fill)
                        .style(iced::theme::Container::Box)
                        .padding(10)
                    )
            )
            .push(
                Container::new(
                    Column::new()
                        .spacing(10)
                        .push(Text::new("Strategy Performance Details").size(16))
                        .push(performance_table)
                )
                .width(Length::Fill)
                .style(iced::theme::Container::Box)
                .padding(10)
            )
            .padding(20)
            .into()
    }
    
    /// Get history points for the selected statistic
    fn get_history_points_for_statistic(
        &self,
        simulation: &Simulation,
        stat_type: StatisticType
    ) -> Vec<HistoryPoint> {
        if simulation.statistics.history.is_empty() {
            // Generate sample data for initial display
            return self.generate_sample_data(stat_type);
        }
        
        simulation.statistics.history.iter().map(|point| {
            let value = match stat_type {
                StatisticType::Population => point.population as f32,
                StatisticType::AvgHealth => point.avg_health,
                StatisticType::AvgHunger => point.avg_hunger,
                StatisticType::AvgThirst => point.avg_thirst,
                StatisticType::AvgEnergy => point.avg_energy,
                StatisticType::AvgAge => point.avg_age,
                StatisticType::FoodCount => point.food_count as f32,
                StatisticType::WaterCount => point.water_count as f32,
            };
            
            HistoryPoint {
                tick: point.tick,
                value,
                label: format!("Tick {}", point.tick),
            }
        }).collect()
    }
    
    /// Generate sample data for initial display
    fn generate_sample_data(&self, stat_type: StatisticType) -> Vec<HistoryPoint> {
        let mut sample_data = Vec::new();
        
        // Generate some placeholder data points
        for i in 0..10 {
            let base_value = match stat_type {
                StatisticType::Population => 20.0,
                StatisticType::AvgHealth => 80.0,
                StatisticType::AvgHunger => 50.0,
                StatisticType::AvgThirst => 50.0,
                StatisticType::AvgEnergy => 70.0,
                StatisticType::AvgAge => 30.0,
                StatisticType::FoodCount => 40.0,
                StatisticType::WaterCount => 30.0,
            };
            
            // Add some variation
            let value = base_value * (0.8 + 0.4 * ((i as f32 * 0.7).sin() + 1.0));
            
            sample_data.push(HistoryPoint {
                tick: i * 10,
                value,
                label: format!("Sample {}", i),
            });
        }
        
        sample_data
    }
    
    /// Get resource history points
    fn get_resource_history_points(&self, simulation: &Simulation) -> Vec<HistoryPoint> {
        if simulation.statistics.history.is_empty() {
            // Generate sample resource data
            let mut sample_data = Vec::new();
            
            for i in 0..10 {
                // Food + water combined resources
                let base_value = 70.0;
                let value = base_value * (0.8 + 0.4 * ((i as f32 * 0.5).sin() + 1.0));
                
                sample_data.push(HistoryPoint {
                    tick: i * 10,
                    value,
                    label: format!("Sample {}", i),
                });
            }
            
            return sample_data;
        }
        
        simulation.statistics.history.iter().map(|point| {
            HistoryPoint {
                tick: point.tick,
                value: (point.food_count + point.water_count) as f32,
                label: format!("Tick {}", point.tick),
            }
        }).collect()
    }
    
    /// Get strategy distribution history
    fn get_strategy_distribution(&self, simulation: &Simulation) -> Vec<HashMap<String, usize>> {
        if simulation.statistics.history.is_empty() {
            // Generate sample strategy distribution
            let mut sample_data = Vec::new();
            
            for i in 0..10 {
                let mut distribution = HashMap::new();
                distribution.insert("Balanced".to_string(), 8 + (i % 3));
                distribution.insert("Aggressive".to_string(), 7 - (i % 4));
                distribution.insert("Cautious".to_string(), 5 + (i % 2));
                sample_data.push(distribution);
            }
            
            return sample_data;
        }
        
        simulation.statistics.history.iter()
            .map(|point| point.strategy_distribution.clone())
            .collect()
    }
    
    /// Create a table of strategy performance details
    fn create_strategy_performance_table<'a>(&self, simulation: &'a Simulation) -> Element<'a, Message> {
        let header = Row::new()
            .spacing(10)
            .push(Text::new("Strategy").width(Length::FillPortion(2)))
            .push(Text::new("Count").width(Length::FillPortion(1)))
            .push(Text::new("Avg Age").width(Length::FillPortion(1)))
            .push(Text::new("Avg Health").width(Length::FillPortion(1)))
            .push(Text::new("Max Age").width(Length::FillPortion(1)))
            .push(Text::new("Avg Fitness").width(Length::FillPortion(1)));
        
        // Create column directly without storing rows
        let mut column = Column::new().spacing(5).push(header).push(Rule::horizontal(1));
        
        // Check if there is any performance data
        let performance_data = &simulation.statistics.evolution_metrics.strategy_performance;
        
        if performance_data.is_empty() {
            // Add sample data if no real data is available
            let sample_strategies = ["Balanced", "Aggressive", "Cautious"];
            
            for strategy in &sample_strategies {
                let row = Row::new()
                    .spacing(10)
                    .push(Text::new(*strategy).width(Length::FillPortion(2)))
                    .push(Text::new("7").width(Length::FillPortion(1)))
                    .push(Text::new("42.5").width(Length::FillPortion(1)))
                    .push(Text::new("78.3").width(Length::FillPortion(1)))
                    .push(Text::new("87").width(Length::FillPortion(1)))
                    .push(Text::new("0.68").width(Length::FillPortion(1)));
                
                column = column.push(row);
            }
        } else {
            // Real data is available
            for (strategy, data) in performance_data {
                let row = Row::new()
                    .spacing(10)
                    .push(Text::new(strategy).width(Length::FillPortion(2)))
                    .push(Text::new(data.count.to_string()).width(Length::FillPortion(1)))
                    .push(Text::new(format!("{:.1}", data.avg_age)).width(Length::FillPortion(1)))
                    .push(Text::new(format!("{:.1}", data.avg_health)).width(Length::FillPortion(1)))
                    .push(Text::new(data.max_age.to_string()).width(Length::FillPortion(1)))
                    .push(Text::new(format!("{:.1}", data.avg_fitness)).width(Length::FillPortion(1)));
                
                column = column.push(row);
            }
        }
        
        Scrollable::new(column)
        .height(Length::Fixed(200.0))
        .into()
    }
}