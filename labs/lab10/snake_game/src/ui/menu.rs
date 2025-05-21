// In src/ui/menu.rs
use iced::widget::{button, column, container, row, text};
use iced::{Alignment, Element, Length, Color};

use crate::Message;

/// Menu screen UI component
pub struct Menu;

impl Menu {
    /// Renders the main menu UI
    pub fn view() -> Element<'static, Message> {
        // Create a title with proper styling
        let title = text("Snake Game")
            .size(60)
            .width(Length::Fill)
            .horizontal_alignment(iced::alignment::Horizontal::Center)
            .style(Color::from_rgb(0.0, 0.8, 0.2)); // Green color to match snake theme

        // Create fancy buttons for mode selection
        let single_player_button = button(
            text("Single Player")
                .horizontal_alignment(iced::alignment::Horizontal::Center)
                .size(22),
        )
        .width(Length::Fixed(200.0))
        .padding(15)
        .style(iced::theme::Button::Primary)
        .on_press(Message::StartSinglePlayer);

        let multi_player_button = button(
            text("Two Players")
                .horizontal_alignment(iced::alignment::Horizontal::Center)
                .size(22),
        )
        .width(Length::Fixed(200.0))
        .padding(15)
        .style(iced::theme::Button::Primary)
        .on_press(Message::StartMultiPlayer);
        
        // Create buttons for settings and high scores
        let settings_button = button(
            text("Settings")
                .horizontal_alignment(iced::alignment::Horizontal::Center)
                .size(18),
        )
        .width(Length::Fixed(160.0))
        .padding(12)
        .style(iced::theme::Button::Secondary)
        .on_press(Message::ShowSettings);
        
        let high_scores_button = button(
            text("High Scores")
                .horizontal_alignment(iced::alignment::Horizontal::Center)
                .size(18),
        )
        .width(Length::Fixed(160.0))
        .padding(12)
        .style(iced::theme::Button::Secondary)
        .on_press(Message::ShowHighScores);

        // Group game mode buttons in a row
        let main_button_row = row![single_player_button, multi_player_button]
            .spacing(30)
            .padding(20)
            .width(Length::Fill)
            .align_items(Alignment::Center);
            
        // Group settings and high scores buttons in a row    
        let secondary_button_row = row![settings_button, high_scores_button]
            .spacing(30)
            .padding(10)
            .width(Length::Fill)
            .align_items(Alignment::Center);

        // Create containers for button rows to ensure centering
        let main_button_container = container(main_button_row)
            .width(Length::Fill)
            .center_x();
            
        let secondary_button_container = container(secondary_button_row)
            .width(Length::Fill)
            .center_x();

        // Create instruction text with clear formatting
        let instruction_single = text("Use WASD to move in single player mode.")
            .size(18)
            .width(Length::Fill)
            .horizontal_alignment(iced::alignment::Horizontal::Center);

        let instruction_multi = text("Player 1: WASD, Player 2: Arrow Keys in two-player mode.")
            .size(18)
            .width(Length::Fill)
            .horizontal_alignment(iced::alignment::Horizontal::Center);
            
        let version_text = text("v1.2.0 - Enhanced Edition")
            .size(14)
            .width(Length::Fill)
            .horizontal_alignment(iced::alignment::Horizontal::Center)
            .style(Color::from_rgb(0.5, 0.5, 0.5));

        // Group instructions together
        let instructions = column![instruction_single, instruction_multi]
            .spacing(5)
            .width(Length::Fill)
            .align_items(Alignment::Center);

        // Create an instructions container
        let instructions_container = container(instructions)
            .width(Length::Fill)
            .padding(10);

        // Combine all elements with proper spacing
        let content = column![
            container(title).padding(40),
            main_button_container,
            secondary_button_container,
            instructions_container,
            version_text
        ]
        .spacing(20)
        .width(Length::Fill)
        .align_items(Alignment::Center);

        // Wrap everything in a container to ensure proper layout on all screen sizes
        container(content)
            .width(Length::Fill)
            .height(Length::Fill)
            .center_x()
            .center_y()
            .into()
    }
}