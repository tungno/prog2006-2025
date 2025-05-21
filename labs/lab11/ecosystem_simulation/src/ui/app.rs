use iced::{
    Application, Command, Element, Length, Settings, Theme, Subscription,
    widget::{Column, Container, Text},
    executor,
};
use iced::time;
use std::time::{Duration, Instant};

use crate::ui::{
    Message, Page,
    simulation_view::SimulationView,
    controls::ControlsPanel,
    heuristic_editor::HeuristicEditor,
    statistics_view::StatisticsView,
};

use crate::simulation::Simulation;
use crate::constants::{GRID_SIZE, CELL_SIZE};

/// The main application state
pub struct App {
    pub simulation: Simulation,
    pub active_page: Page,
    pub simulation_view: SimulationView,
    pub controls_panel: ControlsPanel,
    pub heuristic_editor: HeuristicEditor,
    pub statistics_view: StatisticsView,
    pub running: bool,
    pub tick_speed: u64,
    pub last_tick: Instant,
}

impl Application for App {
    type Executor = executor::Default;
    type Message = Message;
    type Theme = Theme;
    type Flags = ();

    fn new(_flags: ()) -> (Self, Command<Message>) {
        let simulation = Simulation::new();
        let active_page = Page::Simulation;
        
        let controls_panel = ControlsPanel::new();
        
        // Initialize SimulationView state from ControlsPanel
        let mut simulation_view = SimulationView::new(GRID_SIZE, CELL_SIZE as u16);
        simulation_view.update_from_simulation(&simulation);
        
        // Sync visualization options
        simulation_view.set_show_sensing_radius(controls_panel.show_sensing_radius);
        simulation_view.set_show_agent_paths(controls_panel.show_agent_paths);
        simulation_view.set_show_heatmap(controls_panel.show_heatmap);
        simulation_view.set_heatmap_type(controls_panel.heatmap_type);
        
        let heuristic_editor = HeuristicEditor::new(&simulation.strategies);
        let statistics_view = StatisticsView::new();
        
        (
            Self {
                simulation,
                active_page,
                simulation_view,
                controls_panel,
                heuristic_editor,
                statistics_view,
                running: false,
                tick_speed: 200,
                last_tick: Instant::now(),
            },
            Command::none(),
        )
    }

    fn title(&self) -> String {
        "Ecosystem Simulation".to_string()
    }

    fn update(&mut self, message: Message) -> Command<Message> {
        match message {
            Message::Tick(now) => {
                if self.running {
                    let elapsed = now.duration_since(self.last_tick);
                    if elapsed >= Duration::from_millis(self.tick_speed) {
                        self.simulation.update();
                        self.last_tick = now;
                        
                        // Update UI components with new simulation state
                        self.simulation_view.update_from_simulation(&self.simulation);
                        self.controls_panel.update_from_simulation(&self.simulation);
                        self.statistics_view.update_from_simulation(&self.simulation);
                    }
                }
                Command::none()
            }
            Message::ToggleRunning => {
                self.running = !self.running;
                // Update the simulation's running state to match the App's running state
                self.simulation.set_running(self.running);
                if self.running {
                    self.last_tick = Instant::now();
                }
                Command::none()
            }
            Message::Reset => {
                self.running = false;
                self.simulation = Simulation::new();
                // Ensure the new simulation's running state is set correctly
                self.simulation.set_running(false);
                self.simulation_view.update_from_simulation(&self.simulation);
                self.controls_panel.update_from_simulation(&self.simulation);
                self.statistics_view.update_from_simulation(&self.simulation);
                self.heuristic_editor = HeuristicEditor::new(&self.simulation.strategies);
                Command::none()
            }
            Message::SetTickSpeed(speed) => {
                self.tick_speed = speed;
                self.controls_panel.set_tick_speed(speed); // Update control panel state
                Command::none()
            }
            Message::SetActivePage(page) => {
                self.active_page = page;
                Command::none()
            }
            Message::CellClicked(x, y) => {
                self.simulation.select_agent_at(x, y);
                self.simulation_view.update_selected_agent(self.simulation.selected_agent());
                Command::none()
            }
            Message::SetFoodRegenerationRate(rate) => {
                self.simulation.set_food_regeneration_rate(rate);
                self.controls_panel.update_from_simulation(&self.simulation); // Update control panel with new rate
                Command::none()
            }
            Message::SetWaterRegenerationRate(rate) => {
                self.simulation.set_water_regeneration_rate(rate);
                self.controls_panel.update_from_simulation(&self.simulation); // Update control panel with new rate
                Command::none()
            }
            Message::ToggleShowSensingRadius => {
                self.simulation_view.toggle_show_sensing_radius();
                // Update control panel state
                let show_sensing = self.simulation_view.is_show_sensing_radius();
                self.controls_panel.set_show_sensing_radius(show_sensing);
                Command::none()
            }
            Message::ToggleShowAgentPaths => {
                self.simulation_view.toggle_show_agent_paths();
                // Update control panel state
                let show_paths = self.simulation_view.is_show_agent_paths();
                self.controls_panel.set_show_agent_paths(show_paths);
                Command::none()
            }
            Message::ToggleShowHeatmap => {
                self.simulation_view.toggle_show_heatmap();
                // Update control panel state
                let show_heatmap = self.simulation_view.is_show_heatmap();
                self.controls_panel.set_show_heatmap(show_heatmap);
                Command::none()
            }
            Message::SetHeatmapType(heatmap_type) => {
                self.simulation_view.set_heatmap_type(heatmap_type);
                // Update control panel state
                self.controls_panel.set_heatmap_type(heatmap_type);
                Command::none()
            }
            Message::TriggerEvolution => {
                if !self.running {
                    self.simulation.trigger_evolution();
                    self.simulation_view.update_from_simulation(&self.simulation);
                    self.statistics_view.update_from_simulation(&self.simulation);
                }
                Command::none()
            }
            Message::TrackAgent(agent_id) => {
                self.simulation.track_agent(&agent_id);
                self.simulation_view.update_tracked_agents(self.simulation.tracked_agents());
                Command::none()
            }
            Message::ClearTrackedAgents => {
                self.simulation.clear_tracked_agents();
                self.simulation_view.update_tracked_agents(Vec::new());
                Command::none()
            }
            Message::CreateStrategy(name) => {
                self.simulation.create_strategy(&name);
                self.heuristic_editor = HeuristicEditor::new(&self.simulation.strategies);
                Command::none()
            }
            Message::DeleteStrategy(name) => {
                self.simulation.delete_strategy(&name);
                self.heuristic_editor = HeuristicEditor::new(&self.simulation.strategies);
                Command::none()
            }
            Message::EditStrategy(name) => {
                self.heuristic_editor.start_editing(&name, self.simulation.get_strategy(&name));
                Command::none()
            }
            Message::UpdateStrategy(params) => {
                self.simulation.update_strategy(params);
                self.heuristic_editor = HeuristicEditor::new(&self.simulation.strategies);
                Command::none()
            }
            Message::CancelStrategyEdit => {
                self.heuristic_editor.cancel_editing();
                Command::none()
            }
            Message::ImportStrategies(strategies) => {
                self.simulation.import_strategies(strategies);
                self.heuristic_editor = HeuristicEditor::new(&self.simulation.strategies);
                Command::none()
            }
            Message::ExportStrategies => {
                // Export current strategies to a file
                if let Err(e) = self.simulation.export_strategies() {
                    eprintln!("Error exporting strategies: {}", e);
                }
                Command::none()
            }
            Message::SelectStatisticType(stat_type) => {
                self.statistics_view.select_statistic_type(stat_type);
                Command::none()
            }
        }
    }

    fn subscription(&self) -> Subscription<Message> {
        time::every(Duration::from_millis(16)).map(Message::Tick)
    }

    fn view(&self) -> Element<Message> {
        let content = match self.active_page {
            Page::Simulation => {
                let mut column = Column::new()
                    .spacing(20)
                    .width(Length::Fill);
                
                // Make sure the simulation view gets enough space
                let simulation_row = iced::widget::Row::new()
                    .spacing(20)
                    .push(self.controls_panel.view())
                    .push(
                        Container::new(self.simulation_view.view())
                            .width(Length::Fill)
                            .height(Length::Fill)
                            .center_x()
                            .center_y()
                    );
                
                // Add the legend with more width and ensure it's visible
                let legend = Container::new(
                    self.simulation_view.view_legend(&self.simulation.strategies)
                )
                .width(Length::Fill)
                .padding(5);
                
                column = column
                    .push(simulation_row)
                    .push(legend);
                
                column.into()
            },
            Page::HeuristicEditor => {
                self.heuristic_editor.view()
            },
            Page::Statistics => {
                self.statistics_view.view(&self.simulation)
            }
        };

        let tabs = iced::widget::Row::new()
            .spacing(5)
            .push(
                iced::widget::Button::new(Text::new("Simulation"))
                    .on_press(Message::SetActivePage(Page::Simulation))
                    .style(if self.active_page == Page::Simulation {
                        iced::theme::Button::Primary
                    } else {
                        iced::theme::Button::Secondary
                    })
                    .width(Length::Fixed(120.0))
            )
            .push(
                iced::widget::Button::new(Text::new("Heuristics Editor"))
                    .on_press(Message::SetActivePage(Page::HeuristicEditor))
                    .style(if self.active_page == Page::HeuristicEditor {
                        iced::theme::Button::Primary
                    } else {
                        iced::theme::Button::Secondary
                    })
                    .width(Length::Fixed(120.0))
            )
            .push(
                iced::widget::Button::new(Text::new("Statistics"))
                    .on_press(Message::SetActivePage(Page::Statistics))
                    .style(if self.active_page == Page::Statistics {
                        iced::theme::Button::Primary
                    } else {
                        iced::theme::Button::Secondary
                    })
                    .width(Length::Fixed(120.0))
            );

        let main_column = Column::new()
            .spacing(20)
            .push(Text::new("Ecosystem Simulation").size(28))
            .push(tabs)
            .push(content);

        Container::new(main_column)
            .width(Length::Fill)
            .height(Length::Fill)
            .padding(20)
            .center_x()
            .into()
    }
}