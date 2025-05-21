use iced::widget::{button, column, container, row, scrollable, text};
use iced::{Alignment, Element, Length};

use crate::Message;
use crate::utils::storage::{get_storage, HighScoreEntry};

// High scores screen UI component
pub struct HighScores;

impl HighScores {
    // Render the high scores view
    pub fn view() -> Element<'static, Message> {
        let title = text("High Scores")
            .size(30)
            .width(Length::Fill)
            .horizontal_alignment(iced::alignment::Horizontal::Center);

        // Get high scores from storage
        let high_scores = if let Some(storage) = get_storage() {
            storage.load_game_data().high_scores
        } else {
            Vec::new()
        };

        // Create header row
        let header_row = row![
            text("Rank")
                .size(18)
                .width(Length::FillPortion(1))
                .horizontal_alignment(iced::alignment::Horizontal::Center),
            text("Player")
                .size(18)
                .width(Length::FillPortion(3))
                .horizontal_alignment(iced::alignment::Horizontal::Left),
            text("Score")
                .size(18)
                .width(Length::FillPortion(2))
                .horizontal_alignment(iced::alignment::Horizontal::Right),
            text("Date")
                .size(18)
                .width(Length::FillPortion(3))
                .horizontal_alignment(iced::alignment::Horizontal::Right),
        ]
        .spacing(10)
        .padding(10);

        // Create score rows
        let mut score_rows = column![header_row].spacing(5);

        if high_scores.is_empty() {
            // No high scores yet
            score_rows = score_rows.push(
                container(
                    text("No high scores yet. Start playing to set records!")
                        .size(16)
                        .horizontal_alignment(iced::alignment::Horizontal::Center)
                )
                .width(Length::Fill)
                .center_x()
                .padding(20)
            );
        } else {
            // Add each high score to the list
            for (i, entry) in high_scores.iter().enumerate() {
                // Highlight top 3 scores with different styles
                let row_style = match i {
                    0 => iced::theme::Container::Custom(Box::new(GoldStyle)),
                    1 => iced::theme::Container::Custom(Box::new(SilverStyle)),
                    2 => iced::theme::Container::Custom(Box::new(BronzeStyle)),
                    _ => iced::theme::Container::Box,
                };

                let score_row = row![
                    text(format!("#{}", i + 1))
                        .size(16)
                        .width(Length::FillPortion(1))
                        .horizontal_alignment(iced::alignment::Horizontal::Center),
                    text(&entry.player_name)
                        .size(16)
                        .width(Length::FillPortion(3))
                        .horizontal_alignment(iced::alignment::Horizontal::Left),
                    text(format!("{}", entry.score))
                        .size(16)
                        .width(Length::FillPortion(2))
                        .horizontal_alignment(iced::alignment::Horizontal::Right),
                    text(&entry.date)
                        .size(16)
                        .width(Length::FillPortion(3))
                        .horizontal_alignment(iced::alignment::Horizontal::Right),
                ]
                .spacing(10)
                .padding(10);

                // Add the row to the column with the appropriate style
                score_rows = score_rows.push(
                    container(score_row)
                        .width(Length::Fill)
                        .style(row_style)
                );
            }
        }

        // Create a scrollable container for the scores
        let scores_scroll = scrollable(score_rows)
            .height(Length::Fill)
            .width(Length::Fill);

        // Return to menu button
        let menu_button = button(text("Return to Menu"))
            .on_press(Message::ReturnToMenu)
            .padding(10)
            .width(Length::Fixed(150.0));
            
        // Clear high scores button
        let clear_button = button(text("Clear High Scores"))
            .on_press(Message::ClearHighScores)
            .padding(10)
            .width(Length::Fixed(150.0));
            
        // Button row
        let button_row = row![menu_button, clear_button]
            .spacing(20)
            .padding(10)
            .width(Length::Fill)
            .align_items(Alignment::Center);

        // Combine all elements
        let content = column![
            title,
            container(scores_scroll).height(Length::FillPortion(4)).padding(20),
            container(button_row).center_x()
        ]
        .spacing(20)
        .padding(20)
        .width(Length::Fill)
        .height(Length::Fill)
        .align_items(Alignment::Center);

        container(content)
            .width(Length::Fill)
            .height(Length::Fill)
            .center_x()
            .center_y()
            .into()
    }
}

// Custom styles for high scores
pub struct GoldStyle;
pub struct SilverStyle;
pub struct BronzeStyle;

impl iced::widget::container::StyleSheet for GoldStyle {
    type Style = iced::Theme;

    fn appearance(&self, _style: &Self::Style) -> iced::widget::container::Appearance {
        iced::widget::container::Appearance {
            background: Some(iced::Background::Color(iced::Color::from_rgb(1.0, 0.84, 0.0))),
            text_color: Some(iced::Color::BLACK),
            border_radius: 5.0.into(),
            border_width: 0.0,
            border_color: iced::Color::TRANSPARENT,
        }
    }
}

impl iced::widget::container::StyleSheet for SilverStyle {
    type Style = iced::Theme;

    fn appearance(&self, _style: &Self::Style) -> iced::widget::container::Appearance {
        iced::widget::container::Appearance {
            background: Some(iced::Background::Color(iced::Color::from_rgb(0.75, 0.75, 0.75))),
            text_color: Some(iced::Color::BLACK),
            border_radius: 5.0.into(),
            border_width: 0.0,
            border_color: iced::Color::TRANSPARENT,
        }
    }
}

impl iced::widget::container::StyleSheet for BronzeStyle {
    type Style = iced::Theme;

    fn appearance(&self, _style: &Self::Style) -> iced::widget::container::Appearance {
        iced::widget::container::Appearance {
            background: Some(iced::Background::Color(iced::Color::from_rgb(0.8, 0.5, 0.2))),
            text_color: Some(iced::Color::BLACK),
            border_radius: 5.0.into(),
            border_width: 0.0,
            border_color: iced::Color::TRANSPARENT,
        }
    }
}