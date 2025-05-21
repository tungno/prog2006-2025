// In src/ui/game_view.rs
use iced::widget::{container, column, row, text, button, canvas};
use iced::{Alignment, Element, Length, Color, Point, Size, Rectangle};
use iced::widget::canvas::{Path, Stroke, Frame, Geometry};
use iced::mouse;

use crate::Message;
use crate::state::single_player_state::SinglePlayerState;
use crate::state::multi_player_state::MultiPlayerState;
use crate::utils::constants::{CELL_SIZE, GRID_SIZE, SNAKE1_COLOR, SNAKE2_COLOR, FOOD_COLOR, GRID_COLOR, GRID_LINE_COLOR, SNAKE_HEAD_COLOR};
use crate::logic::snake::Snake;
use crate::logic::food::Food;

/// Game view UI component that renders the active game
pub struct GameView;

/// Canvas state for single player game
struct SinglePlayerCanvas {
    state: SinglePlayerState,
    paused: bool,
}

/// Canvas state for multi-player game
struct MultiPlayerCanvas {
    state: MultiPlayerState,
    paused: bool,
}

impl canvas::Program<Message> for SinglePlayerCanvas {
    type State = ();

    fn draw(
        &self,
        _state: &Self::State,
        renderer: &iced::Renderer,
        _theme: &iced::Theme,
        bounds: Rectangle,
        _cursor: mouse::Cursor,
    ) -> Vec<Geometry> {
        // Create a frame for drawing
        let mut frame = Frame::new(renderer, bounds.size());
        
        // Draw grid background
        let background = Path::rectangle(
            Point::new(0.0, 0.0),
            Size::new(GRID_SIZE as f32 * CELL_SIZE, GRID_SIZE as f32 * CELL_SIZE),
        );
        frame.fill(&background, GRID_COLOR);

        // Draw grid lines
        for i in 0..=GRID_SIZE {
            let position = i as f32 * CELL_SIZE;

            // Horizontal line
            frame.stroke(
                &Path::line(
                    Point::new(0.0, position),
                    Point::new(GRID_SIZE as f32 * CELL_SIZE, position),
                ),
                Stroke::default().with_color(GRID_LINE_COLOR).with_width(1.0),
            );

            // Vertical line
            frame.stroke(
                &Path::line(
                    Point::new(position, 0.0),
                    Point::new(position, GRID_SIZE as f32 * CELL_SIZE),
                ),
                Stroke::default().with_color(GRID_LINE_COLOR).with_width(1.0),
            );
        }

        // Draw snake body
        for (i, segment) in self.state.snake.body.iter().enumerate() {
            let x = segment.x as f32 * CELL_SIZE;
            let y = segment.y as f32 * CELL_SIZE;
            
            let segment_color = if i == 0 { SNAKE_HEAD_COLOR } else { SNAKE1_COLOR };
            
            let segment_path = Path::rectangle(
                Point::new(x, y),
                Size::new(CELL_SIZE, CELL_SIZE),
            );
            
            frame.fill(&segment_path, segment_color);
        }

        // Draw food
        let food_x = self.state.food.position.x as f32 * CELL_SIZE;
        let food_y = self.state.food.position.y as f32 * CELL_SIZE;
        let food_path = Path::rectangle(
            Point::new(food_x, food_y),
            Size::new(CELL_SIZE, CELL_SIZE),
        );
        frame.fill(&food_path, FOOD_COLOR);

        // Draw pause overlay if game is paused
        if self.paused {
            let overlay = Path::rectangle(
                Point::new(0.0, 0.0),
                Size::new(GRID_SIZE as f32 * CELL_SIZE, GRID_SIZE as f32 * CELL_SIZE),
            );
            frame.fill(&overlay, Color {
                r: 0.0, g: 0.0, b: 0.0, a: 0.5
            });
        }
        
        // Return geometry
        vec![frame.into_geometry()]
    }
}

impl canvas::Program<Message> for MultiPlayerCanvas {
    type State = ();

    fn draw(
        &self,
        _state: &Self::State,
        renderer: &iced::Renderer,
        _theme: &iced::Theme,
        bounds: Rectangle,
        _cursor: mouse::Cursor,
    ) -> Vec<Geometry> {
        // Create a frame for drawing
        let mut frame = Frame::new(renderer, bounds.size());
        
        // Draw grid background
        let background = Path::rectangle(
            Point::new(0.0, 0.0),
            Size::new(GRID_SIZE as f32 * CELL_SIZE, GRID_SIZE as f32 * CELL_SIZE),
        );
        frame.fill(&background, GRID_COLOR);

        // Draw grid lines
        for i in 0..=GRID_SIZE {
            let position = i as f32 * CELL_SIZE;

            // Horizontal line
            frame.stroke(
                &Path::line(
                    Point::new(0.0, position),
                    Point::new(GRID_SIZE as f32 * CELL_SIZE, position),
                ),
                Stroke::default().with_color(GRID_LINE_COLOR).with_width(1.0),
            );

            // Vertical line
            frame.stroke(
                &Path::line(
                    Point::new(position, 0.0),
                    Point::new(position, GRID_SIZE as f32 * CELL_SIZE),
                ),
                Stroke::default().with_color(GRID_LINE_COLOR).with_width(1.0),
            );
        }

        // Draw snake 1 (player 1)
        if self.state.alive1 {
            for (i, segment) in self.state.snake1.body.iter().enumerate() {
                let x = segment.x as f32 * CELL_SIZE;
                let y = segment.y as f32 * CELL_SIZE;
                
                let segment_color = if i == 0 { SNAKE_HEAD_COLOR } else { SNAKE1_COLOR };
                
                let segment_path = Path::rectangle(
                    Point::new(x, y),
                    Size::new(CELL_SIZE, CELL_SIZE),
                );
                
                frame.fill(&segment_path, segment_color);
            }
        }

        // Draw snake 2 (player 2)
        if self.state.alive2 {
            for (i, segment) in self.state.snake2.body.iter().enumerate() {
                let x = segment.x as f32 * CELL_SIZE;
                let y = segment.y as f32 * CELL_SIZE;
                
                let segment_color = if i == 0 { SNAKE_HEAD_COLOR } else { SNAKE2_COLOR };
                
                let segment_path = Path::rectangle(
                    Point::new(x, y),
                    Size::new(CELL_SIZE, CELL_SIZE),
                );
                
                frame.fill(&segment_path, segment_color);
            }
        }

        // Draw food
        let food_x = self.state.food.position.x as f32 * CELL_SIZE;
        let food_y = self.state.food.position.y as f32 * CELL_SIZE;
        let food_path = Path::rectangle(
            Point::new(food_x, food_y),
            Size::new(CELL_SIZE, CELL_SIZE),
        );
        frame.fill(&food_path, FOOD_COLOR);

        // Draw overlay for paused game or when a player wins
        if self.paused || self.state.winner.is_some() {
            let overlay = Path::rectangle(
                Point::new(0.0, 0.0),
                Size::new(GRID_SIZE as f32 * CELL_SIZE, GRID_SIZE as f32 * CELL_SIZE),
            );
            frame.fill(&overlay, Color {
                r: 0.0, g: 0.0, b: 0.0, a: 0.5
            });
        }
        
        // Return geometry
        vec![frame.into_geometry()]
    }
}

impl GameView {
    /// Renders the single player game view
    pub fn view_single_player(state: &SinglePlayerState, paused: bool) -> Element<'static, Message> {
        // Load settings to get player name
        let settings = if let Some(storage) = crate::utils::storage::get_storage() {
            storage.load_settings()
        } else {
            crate::utils::storage::GameSettings::default()
        };
        
        // Create a clone of the state that we'll use in our canvas
        let state_clone = SinglePlayerState {
            snake: Snake {
                body: state.snake.body.clone(),
                direction: state.snake.direction,
                should_grow: state.snake.should_grow,
            },
            food: Food {
                position: state.food.position,
            },
            score: state.score,
            high_score: state.high_score,
            game_over: state.game_over,
        };

        // Create a consistent header with score information and player name
        let header = row![
            text(format!("{} - Score: {}", settings.player1_name, state.score))
                .size(24)
                .width(Length::Fill),
            text(format!("High Score: {}", state.high_score))
                .size(18)
                .width(Length::FillPortion(1)),
            text("Press P to Pause")
                .size(16)
                .width(Length::FillPortion(1))
        ]
        .spacing(20)
        .padding(10)
        .width(Length::Fill)
        .align_items(Alignment::Center);

        // Create the game canvas
        let game_canvas = canvas(SinglePlayerCanvas {
            state: state_clone,
            paused,
        })
        .width(Length::Fixed(GRID_SIZE as f32 * CELL_SIZE))
        .height(Length::Fixed(GRID_SIZE as f32 * CELL_SIZE));

        // Controls explanation with player name
        let controls = text(format!("Controls for {}: W (up), A (left), S (down), D (right)", settings.player1_name))
            .size(16)
            .width(Length::Fill)
            .horizontal_alignment(iced::alignment::Horizontal::Center);

        // Return to menu button - Made more visible and always present
        let menu_button = button(
            text("Return to Menu")
                .horizontal_alignment(iced::alignment::Horizontal::Center)
                .size(16)
        )
        .width(Length::Fixed(150.0))
        .padding(10)
        .on_press(Message::ReturnToMenu);

        // Create a button container to ensure the button is centered
        let button_container = container(menu_button)
            .width(Length::Fill)
            .center_x();

        // Combine all elements
        let content = column![
            header,
            container(game_canvas)
                .width(Length::Fill)
                .center_x(),
            controls,
            button_container
        ]
        .spacing(20)
        .padding(20)
        .width(Length::Fill)
        .align_items(Alignment::Center);

        // Make the whole UI responsive by using container with centering
        container(content)
            .width(Length::Fill)
            .height(Length::Fill)
            .center_x()
            .center_y()
            .into()
    }
    
    /// Renders the multi-player game view
    pub fn view_multi_player(state: &MultiPlayerState, paused: bool) -> Element<'static, Message> {
        // Load settings to get player names
        let settings = if let Some(storage) = crate::utils::storage::get_storage() {
            storage.load_settings()
        } else {
            crate::utils::storage::GameSettings::default()
        };
        
        // Create a clone of the state that we'll use in our canvas
        let state_clone = MultiPlayerState {
            snake1: Snake {
                body: state.snake1.body.clone(),
                direction: state.snake1.direction,
                should_grow: state.snake1.should_grow,
            },
            snake2: Snake {
                body: state.snake2.body.clone(),
                direction: state.snake2.direction,
                should_grow: state.snake2.should_grow,
            },
            food: Food {
                position: state.food.position,
            },
            score1: state.score1,
            score2: state.score2,
            alive1: state.alive1,
            alive2: state.alive2,
            winner: state.winner,
            game_over: state.game_over,
        };

        // Create a consistent header with player scores and custom names
        let header = row![
            text(format!("{}: {}", settings.player1_name, state.score1))
                .size(18)
                .style(SNAKE1_COLOR)
                .width(Length::FillPortion(1)),
            text("Two Player Mode")
                .size(24)
                .width(Length::FillPortion(2)),
            text(format!("{}: {}", settings.player2_name, state.score2))
                .size(18)
                .style(SNAKE2_COLOR)
                .width(Length::FillPortion(1))
        ]
        .spacing(20)
        .padding(10)
        .width(Length::Fill)
        .align_items(Alignment::Center);

        // Create a consistent pause hint
        let pause_hint = container(
            text("Press P to Pause").size(16)
        )
        .width(Length::Fill)
        .center_x();

        // Status text that won't be empty - use custom player names
        let status_text = if let Some(winner) = state.winner {
            match winner {
                1 => format!("{} is winning!", settings.player1_name),
                2 => format!("{} is winning!", settings.player2_name),
                _ => "Game in progress".to_string(),
            }
        } else if !state.alive1 && !state.alive2 {
            "Both players crashed!".to_string()
        } else if !state.alive1 {
            format!("{} crashed!", settings.player1_name)
        } else if !state.alive2 {
            format!("{} crashed!", settings.player2_name)
        } else {
            "Game in progress".to_string() // Always provide a non-empty string
        };

        // Create a status display
        let status = container(
            text(status_text).size(20)
        )
        .width(Length::Fill)
        .center_x();

        // Create the game canvas
        let game_canvas = canvas(MultiPlayerCanvas {
            state: state_clone,
            paused,
        })
        .width(Length::Fixed(GRID_SIZE as f32 * CELL_SIZE))
        .height(Length::Fixed(GRID_SIZE as f32 * CELL_SIZE));

        // Create a canvas container to ensure the canvas is centered
        let canvas_container = container(game_canvas)
            .width(Length::Fill)
            .center_x();

        // Controls explanation with custom player names
        let controls = container(
            text(format!("{}: WASD, {}: Arrow Keys", settings.player1_name, settings.player2_name)).size(16)
        )
        .width(Length::Fill)
        .center_x();

        // Return to menu button - Made more visible and always present
        let menu_button = button(
            text("Return to Menu")
                .horizontal_alignment(iced::alignment::Horizontal::Center)
                .size(16)
        )
        .width(Length::Fixed(150.0))
        .padding(10)
        .on_press(Message::ReturnToMenu);

        // Create a button container to ensure the button is centered
        let button_container = container(menu_button)
            .width(Length::Fill)
            .center_x();

        // Combine all elements
        let content = column![
            header,
            pause_hint,
            status,
            canvas_container,
            controls,
            button_container
        ]
        .spacing(10)
        .padding(20)
        .width(Length::Fill)
        .align_items(Alignment::Center);

        // Make the whole UI responsive by using container with centering
        container(content)
            .width(Length::Fill)
            .height(Length::Fill)
            .center_x()
            .center_y()
            .into()
    }
}