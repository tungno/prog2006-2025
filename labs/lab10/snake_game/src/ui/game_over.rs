// In src/ui/game_over.rs
use iced::widget::{button, column, container, row, text};
use iced::{Alignment, Element, Length, Color};

use crate::Message;
use crate::utils::constants::{SNAKE1_COLOR, SNAKE2_COLOR};

/// Game over screen UI component
pub struct GameOver;

impl GameOver {
    /// Renders the game over screen for single player mode
    pub fn view_single_player(score: u32, high_score: u32) -> Element<'static, Message> {
        // Load settings to get player name
        let settings = if let Some(storage) = crate::utils::storage::get_storage() {
            storage.load_settings()
        } else {
            crate::utils::storage::GameSettings::default()
        };
        
        // Create a visually striking game over title
        let title = text("Game Over")
            .size(60)
            .width(Length::Fill)
            .horizontal_alignment(iced::alignment::Horizontal::Center)
            .style(Color::from_rgb(0.9, 0.2, 0.2)); // Red color for game over

        // Create score display with clear formatting and player name
        let score_text = text(format!("{}'s Score: {}", settings.player1_name, score))
            .size(30)
            .width(Length::Fill)
            .horizontal_alignment(iced::alignment::Horizontal::Center);

        let high_score_text = text(format!("High Score: {}", high_score))
            .size(24)
            .width(Length::Fill)
            .horizontal_alignment(iced::alignment::Horizontal::Center);

        // Show achievement text if new high score is reached
        let high_score_achievement = if score >= high_score && high_score > 0 {
            text("New High Score!")
                .size(36)
                .width(Length::Fill)
                .horizontal_alignment(iced::alignment::Horizontal::Center)
                .style(Color::from_rgb(1.0, 0.8, 0.0)) // Gold color
        } else {
            text("Try again to beat the high score!")
                .size(24)
                .width(Length::Fill)
                .horizontal_alignment(iced::alignment::Horizontal::Center)
        };

        // Create prominent buttons with consistent styling
        let play_again_button = button(
            text("Play Again")
                .horizontal_alignment(iced::alignment::Horizontal::Center)
                .size(22),
        )
        .width(Length::Fixed(200.0))
        .padding(15)
        .on_press(Message::PlayAgain);

        let menu_button = button(
            text("Main Menu")
                .horizontal_alignment(iced::alignment::Horizontal::Center)
                .size(22),
        )
        .width(Length::Fixed(200.0))
        .padding(15)
        .on_press(Message::ReturnToMenu);

        // Group buttons in a row
        let buttons = row![play_again_button, menu_button]
            .spacing(30)
            .padding(20)
            .width(Length::Fill)
            .align_items(Alignment::Center);

        // Create a button container to ensure centering
        let button_container = container(buttons)
            .width(Length::Fill)
            .center_x();

        // Combine all elements with proper spacing
        let content = column![
            container(title).padding(20),
            score_text,
            high_score_text,
            container(high_score_achievement).padding(20),
            button_container
        ]
        .spacing(10)
        .padding(20)
        .width(Length::Fill)
        .align_items(Alignment::Center);

        // Wrap everything in a container for responsive layout
        container(content)
            .width(Length::Fill)
            .height(Length::Fill)
            .center_x()
            .center_y()
            .into()
    }
    
    /// Renders the game over screen for multi-player mode
    pub fn view_multi_player(score1: u32, score2: u32, winner: Option<u8>) -> Element<'static, Message> {
        // Load settings to get player names
        let settings = if let Some(storage) = crate::utils::storage::get_storage() {
            storage.load_settings()
        } else {
            crate::utils::storage::GameSettings::default()
        };
        
        // Create a visually striking game over title
        let title = text("Game Over")
            .size(60)
            .width(Length::Fill)
            .horizontal_alignment(iced::alignment::Horizontal::Center)
            .style(Color::from_rgb(0.9, 0.2, 0.2)); // Red color for game over

        // Create colored score displays for each player with custom names
        let player1_score = text(format!("{}: {}", settings.player1_name, score1))
            .size(30)
            .style(SNAKE1_COLOR);
            
        let player2_score = text(format!("{}: {}", settings.player2_name, score2))
            .size(30)
            .style(SNAKE2_COLOR);

        // Group scores in a row with proper spacing
        let scores = row![
            player1_score,
            text(" vs ").size(30),
            player2_score
        ]
        .spacing(10)
        .padding(15)
        .width(Length::Fill)
        .align_items(Alignment::Center);
        
        // Create a score container to ensure centering
        let scores_container = container(scores)
            .width(Length::Fill)
            .center_x();

        // Create a prominent winner announcement with custom player names
        let winner_text = match winner {
            Some(1) => text(format!("{} Wins!", settings.player1_name)).size(40).style(SNAKE1_COLOR),
            Some(2) => text(format!("{} Wins!", settings.player2_name)).size(40).style(SNAKE2_COLOR),
            _ => text("It's a Tie!").size(40)
        };
        
        // Create a container for the winner text
        let winner_container = container(winner_text)
            .width(Length::Fill)
            .center_x()
            .padding(20);

        // Create prominent buttons with consistent styling
        let play_again_button = button(
            text("Play Again")
                .horizontal_alignment(iced::alignment::Horizontal::Center)
                .size(22),
        )
        .width(Length::Fixed(200.0))
        .padding(15)
        .on_press(Message::PlayAgain);

        let menu_button = button(
            text("Main Menu")
                .horizontal_alignment(iced::alignment::Horizontal::Center)
                .size(22),
        )
        .width(Length::Fixed(200.0))
        .padding(15)
        .on_press(Message::ReturnToMenu);

        // Group buttons in a row
        let buttons = row![play_again_button, menu_button]
            .spacing(30)
            .padding(20)
            .width(Length::Fill)
            .align_items(Alignment::Center);

        // Create a button container to ensure centering
        let button_container = container(buttons)
            .width(Length::Fill)
            .center_x();

        // Combine all elements with proper spacing
        let content = column![
            container(title).padding(20),
            scores_container,
            winner_container,
            button_container
        ]
        .spacing(10)
        .padding(20)
        .width(Length::Fill)
        .align_items(Alignment::Center);

        // Wrap everything in a container for responsive layout
        container(content)
            .width(Length::Fill)
            .height(Length::Fill)
            .center_x()
            .center_y()
            .into()
    }
}