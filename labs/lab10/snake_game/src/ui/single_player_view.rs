// In src/ui/single_player_view.rs
use iced::widget::{canvas, Canvas};
use iced::{Element, Color, Point, Size};
use iced::widget::canvas::{Cache, Frame, Path, Stroke};
use iced::mouse;

use crate::utils::constants::{CELL_SIZE, GRID_SIZE, SNAKE1_COLOR, FOOD_COLOR, GRID_COLOR, GRID_LINE_COLOR, SNAKE_HEAD_COLOR};
use crate::state::single_player_state::SinglePlayerState;

/// Single player game view
pub struct SinglePlayerView;

/// Canvas State for rendering the single player game
pub struct SinglePlayerCanvas {
    cache: Cache,
    state: SinglePlayerState,
    paused: bool,
}

impl canvas::Program<()> for SinglePlayerCanvas {
    type State = ();

    fn draw(
        &self,
        _state: &Self::State,
        renderer: &iced::Renderer,
        _theme: &iced::Theme,
        bounds: iced::Rectangle,
        _cursor: mouse::Cursor,
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
        
        // Return the frame as a single geometry
        vec![frame.into_geometry()]
    }
}

impl SinglePlayerView {
    /// Renders the single player game canvas
    pub fn render_canvas(paused: bool) -> Element<'static, ()> {
        let state = SinglePlayerState::new(); // In a real app, you'd get this from app state
        
        Canvas::new(SinglePlayerCanvas {
            cache: Cache::default(),
            state,
            paused,
        })
        .width(GRID_SIZE as f32 * CELL_SIZE)
        .height(GRID_SIZE as f32 * CELL_SIZE)
        .into()
    }
}