mod constants;
mod environment;
mod agents;
mod heuristics;
mod evolution;
mod statistics;
mod ui;
mod simulation;

use iced::{Application, Settings};
use ui::app::App;

fn main() -> iced::Result {
    // Initialize logging if needed
    // std::env::set_var("RUST_LOG", "debug");
    // env_logger::init();
    
    App::run(Settings {
        window: iced::window::Settings {
            size: (1024, 768),
            resizable: true,
            decorations: true,
            ..Default::default()
        },
        antialiasing: true,
        ..Default::default()
    })
}