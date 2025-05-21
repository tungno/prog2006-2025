use iced::widget::canvas::{self, Canvas, Frame, Path, Stroke, LineCap, LineJoin, LineDash};
use iced::{Element, Length, Color, Rectangle, Point, Size, Theme};
use std::collections::HashMap;
use std::ops::Range;

use crate::statistics::history::HistoryPoint;
use crate::statistics::collector::StatisticType;

/// The chart model that stores the data and configuration
pub struct Chart {
    data: Vec<HistoryPoint>,
    title: String,
    y_range: Range<f32>,
    width: f32,
    height: f32,
    color: Color,
}

impl Chart {
    /// Create a new chart with the specified data
    pub fn new(data: Vec<HistoryPoint>, title: &str, color: Color) -> Self {
        // Calculate the y-range based on data, or use defaults
        let y_range = if data.is_empty() {
            0.0..100.0
        } else {
            let min = data.iter()
                .map(|p| p.value)
                .fold(f32::INFINITY, f32::min);
            let max = data.iter()
                .map(|p| p.value)
                .fold(f32::NEG_INFINITY, f32::max);
            
            // Ensure a reasonable range, even for flat data
            if min == max {
                if min == 0.0 {
                    0.0..1.0
                } else {
                    min * 0.9..min * 1.1
                }
            } else {
                // Add padding
                let padding = (max - min) * 0.1;
                (min - padding).max(0.0)..(max + padding)
            }
        };
        
        Self {
            data,
            title: title.to_string(),
            y_range,
            width: 400.0,
            height: 200.0,
            color,
        }
    }
    
    /// Set the dimensions of the chart
    pub fn dimensions(mut self, width: f32, height: f32) -> Self {
        self.width = width;
        self.height = height;
        self
    }
    
    /// Set a custom y-axis range
    pub fn y_range(mut self, min: f32, max: f32) -> Self {
        self.y_range = min..max;
        self
    }
    
    /// View the chart as an Iced UI element
    pub fn view(&self) -> Element<'_, crate::ui::Message> {
        Canvas::new(self)
            .width(Length::Fixed(self.width))
            .height(Length::Fixed(self.height))
            .into()
    }
}

impl canvas::Program<crate::ui::Message> for Chart {
    type State = ();
    
    fn draw(
        &self,
        _state: &Self::State,
        renderer: &iced::Renderer,
        _theme: &Theme,
        bounds: Rectangle,
        _cursor: iced::mouse::Cursor,
    ) -> Vec<canvas::Geometry> {
        let mut frame = Frame::new(renderer, bounds.size());

        
        // If we have no data, draw empty chart
        if self.data.is_empty() {
            return vec![frame.into_geometry()];
        }
        
        // Calculate layout parameters
        let padding = 40.0;
        let plot_width = bounds.width - padding * 2.0;
        let plot_height = bounds.height - padding * 2.0;
        
        // Draw title
        let title_text = canvas::Text {
            content: self.title.clone(),
            position: Point::new(bounds.width / 2.0, padding / 2.0),
            color: Color::BLACK,
            size: 16.0,
            horizontal_alignment: iced::alignment::Horizontal::Center,
            vertical_alignment: iced::alignment::Vertical::Center,
            ..canvas::Text::default()
        };
        frame.fill_text(title_text);
        
        // Draw axes
        let axes = Path::new(|path| {
            // X-axis
            path.move_to(Point::new(padding, bounds.height - padding));
            path.line_to(Point::new(bounds.width - padding, bounds.height - padding));
            
            // Y-axis
            path.move_to(Point::new(padding, bounds.height - padding));
            path.line_to(Point::new(padding, padding));
        });
        
        let axes_stroke = Stroke {
            style: iced::widget::canvas::Style::Solid(Color::BLACK),
            width: 1.0,
            line_cap: LineCap::Round,
            line_join: LineJoin::Round,
            line_dash: LineDash::default(),
        };
        
        frame.stroke(&axes, axes_stroke);
        
        // Draw Y-axis labels
        let y_min = self.y_range.start;
        let y_max = self.y_range.end;
        let y_range = y_max - y_min;
        
        let y_step = if y_range <= 10.0 {
            1.0
        } else if y_range <= 50.0 {
            5.0
        } else if y_range <= 100.0 {
            10.0
        } else if y_range <= 500.0 {
            50.0
        } else if y_range <= 1000.0 {
            100.0
        } else {
            500.0
        };
        
        let mut y = (y_min / y_step).ceil() * y_step;
        while y <= y_max {
            let y_pos = bounds.height - padding - (y - y_min) / y_range * plot_height;
            
            // Draw grid line
            let grid = Path::new(|path| {
                path.move_to(Point::new(padding, y_pos));
                path.line_to(Point::new(bounds.width - padding, y_pos));
            });
            
            let grid_stroke = Stroke {
                style: iced::widget::canvas::Style::Solid(Color::from_rgba8(200, 200, 200, 0.5)),
                width: 0.5,
                line_cap: LineCap::Round,
                line_join: LineJoin::Round,
                line_dash: LineDash::default(),
            };
            
            frame.stroke(&grid, grid_stroke);
            
            // Draw label
            let label = canvas::Text {
                content: format!("{:.0}", y),
                position: Point::new(padding - 5.0, y_pos),
                color: Color::BLACK,
                size: 12.0,
                horizontal_alignment: iced::alignment::Horizontal::Right,
                vertical_alignment: iced::alignment::Vertical::Center,
                ..canvas::Text::default()
            };
            
            frame.fill_text(label);
            
            y += y_step;
        }
        
        // Draw X-axis labels and grid
        let num_x_labels = 5.min(self.data.len());
        if num_x_labels > 0 {
            let step = self.data.len() / num_x_labels;
            
            for i in 0..num_x_labels {
                let idx = i * step;
                if idx < self.data.len() {
                    let point = &self.data[idx];
                    let x_pos = padding + (idx as f32 / (self.data.len() - 1) as f32) * plot_width;
                    
                    // Draw grid line
                    let grid = Path::new(|path| {
                        path.move_to(Point::new(x_pos, padding));
                        path.line_to(Point::new(x_pos, bounds.height - padding));
                    });
                    
                    let grid_stroke = Stroke {
                        style: iced::widget::canvas::Style::Solid(Color::from_rgba8(200, 200, 200, 0.5)),
                        width: 0.5,
                        line_cap: LineCap::Round,
                        line_join: LineJoin::Round,
                        line_dash: LineDash::default(),
                    };
                    
                    frame.stroke(&grid, grid_stroke);
                    
                    // Draw label
                    let label = canvas::Text {
                        content: format!("{}", point.tick),
                        position: Point::new(x_pos, bounds.height - padding + 15.0),
                        color: Color::BLACK,
                        size: 12.0,
                        horizontal_alignment: iced::alignment::Horizontal::Center,
                        vertical_alignment: iced::alignment::Vertical::Top,
                        ..canvas::Text::default()
                    };
                    
                    frame.fill_text(label);
                }
            }
        }
        
        // Draw data line
        if self.data.len() >= 2 {
            let line = Path::new(|path| {
                let first = &self.data[0];
                let x = padding;
                let y = bounds.height - padding - (first.value - y_min) / y_range * plot_height;
                path.move_to(Point::new(x, y));
                
                for (i, point) in self.data.iter().enumerate().skip(1) {
                    let x = padding + (i as f32 / (self.data.len() - 1) as f32) * plot_width;
                    let y = bounds.height - padding - (point.value - y_min) / y_range * plot_height;
                    path.line_to(Point::new(x, y));
                }
            });
            
            let line_stroke = Stroke {
                style: iced::widget::canvas::Style::Solid(self.color),
                width: 2.0,
                line_cap: LineCap::Round,
                line_join: LineJoin::Round,
                line_dash: LineDash::default(),
            };
            
            frame.stroke(&line, line_stroke);
        }
        
        vec![frame.into_geometry()]
    }
    
    fn update(
        &self,
        _state: &mut Self::State,
        _event: canvas::Event,
        _bounds: Rectangle,
        _cursor: iced::mouse::Cursor,
    ) -> (canvas::event::Status, Option<crate::ui::Message>) {
        (canvas::event::Status::Ignored, None)
    }
}

/// Create a line chart for a specific statistic type
pub fn create_line_chart(
    data: &[HistoryPoint], 
    statistic: StatisticType, 
    width: f32, 
    height: f32
) -> Chart {
    let color = match statistic {
        StatisticType::Population => Color::from_rgb(0.2, 0.4, 0.8),
        StatisticType::AvgHealth => Color::from_rgb(0.8, 0.2, 0.2),
        StatisticType::AvgHunger => Color::from_rgb(0.2, 0.8, 0.2),
        StatisticType::AvgThirst => Color::from_rgb(0.2, 0.2, 0.8),
        StatisticType::AvgEnergy => Color::from_rgb(0.8, 0.6, 0.2),
        StatisticType::AvgAge => Color::from_rgb(0.5, 0.5, 0.5),
        StatisticType::FoodCount => Color::from_rgb(0.2, 0.8, 0.2),
        StatisticType::WaterCount => Color::from_rgb(0.2, 0.2, 0.8),
    };
    
    Chart::new(data.to_vec(), statistic.label(), color)
        .dimensions(width, height)
}

/// Create a strategy distribution chart
pub fn create_strategy_chart(
    data: &[HashMap<String, usize>],
    width: f32,
    height: f32
) -> Chart {
    // Transform data into a format suitable for charting
    let mut points = Vec::new();
    
    if !data.is_empty() {
        // Get all strategy names
        let mut all_strategies = Vec::new();
        for dist in data {
            for strategy in dist.keys() {
                if !all_strategies.contains(strategy) {
                    all_strategies.push(strategy.clone());
                }
            }
        }
        
        // Create a data point for the total number of agents in each tick
        for (i, dist) in data.iter().enumerate() {
            let total: usize = dist.values().sum();
            
            points.push(HistoryPoint {
                tick: i,
                value: total as f32,
                label: format!("Tick {}", i),
            });
        }
    }
    
    Chart::new(points, "Strategy Distribution", Color::from_rgb(0.5, 0.2, 0.7))
        .dimensions(width, height)
}