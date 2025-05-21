use iced::widget::{canvas, Canvas};
use iced::{Element, Color, Point, Size};
use iced::widget::canvas::{Cache, Frame, Path, Stroke};
use iced::mouse;

use crate::utils::constants::{CELL_SIZE, GRID_SIZE, SNAKE1_COLOR, SNAKE2_COLOR, FOOD_COLOR, GRID_COLOR, GRID_LINE_COLOR, SNAKE_HEAD_COLOR};
use crate::state::multi_player_state::MultiPlayerState;

/// Multi-player game view
pub struct MultiPlayerView;

/// Canvas State for rendering the multi-player game
pub struct MultiPlayerCanvas {
    cache: Cache,
    state: MultiPlayerState,
    paused: bool,
}

impl canvas::Program<()> for MultiPlayerCanvas {
    type State = ();

    fn draw(
        &self,
        _state: &Self::State,
        renderer: &iced::Renderer,
        theme: &iced::Theme,
        bounds: iced::Rectangle,
        cursor: mouse::Cursor,
    ) -> Vec<canvas::Geometry> {
        // Create a frame and use it to draw
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

            // Draw status text (PAUSED or PLAYER X WINS)
            // Note: In a real implementation, you would use a text renderer
            // This is a simplified version for the example
        }
        
        // Return the frame as a single geometry
        vec![frame.into_geometry()]
    }
}

impl MultiPlayerView {
    /// Renders the multi-player game canvas
    pub fn render_canvas(paused: bool) -> Element<'static, ()> {
        let state = MultiPlayerState::new(); // In a real app, you'd get this from app state
        
        Canvas::new(MultiPlayerCanvas {
            cache: Cache::default(),
            state,
            paused,
        })
        .width(GRID_SIZE as f32 * CELL_SIZE)
        .height(GRID_SIZE as f32 * CELL_SIZE)
        .into()
    }
}