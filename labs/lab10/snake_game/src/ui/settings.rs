use iced::widget::{button, checkbox, column, container, horizontal_rule, pick_list, row, slider, text, text_input};
use iced::{Alignment, Element, Length};

use crate::Message;
use crate::utils::storage::{Difficulty, GameSettings};

// Settings screen UI component
pub struct Settings;

#[derive(Debug, Clone)]
pub enum SettingsMessage {
    MusicToggled(bool),
    SoundEffectsToggled(bool),
    MusicVolumeChanged(f32),
    SoundEffectsVolumeChanged(f32),
    DifficultyChanged(Difficulty),
    AnimationsToggled(bool),
    Player1NameChanged(String),
    Player2NameChanged(String),
    SaveSettings,
    Cancel,
}

impl Settings {
    // Render the settings view
    pub fn view(settings: &GameSettings) -> Element<'static, Message> {
        let title = text("Game Settings")
            .size(30)
            .width(Length::Fill)
            .horizontal_alignment(iced::alignment::Horizontal::Center);

        // Audio Settings
        let audio_title = text("Audio Settings")
            .size(24)
            .width(Length::Fill);
            
        // Music toggle
        let music_checkbox = checkbox(
            "Background Music",
            settings.music_enabled,
            |checked| Message::SettingsChanged(SettingsMessage::MusicToggled(checked))
        )
        .size(18)
        .spacing(10);
        
        // Music volume slider (only show if music is enabled)
        let music_volume_slider = if settings.music_enabled {
            slider(0.0..=1.0, settings.music_volume, |value| {
                Message::SettingsChanged(SettingsMessage::MusicVolumeChanged(value))
            })
            .step(0.1)
            .width(Length::Fill)
        } else {
            slider(0.0..=1.0, 0.0, |_| Message::SettingsChanged(SettingsMessage::MusicVolumeChanged(0.0)))
                .step(0.1)
                .width(Length::Fill)
        };
        
        let music_volume_text = text(format!("Music Volume: {:.1}", settings.music_volume * 10.0))
            .size(16);
        
        // Sound effects toggle
        let sfx_checkbox = checkbox(
            "Sound Effects",
            settings.sound_effects_enabled,
            |checked| Message::SettingsChanged(SettingsMessage::SoundEffectsToggled(checked))
        )
        .size(18)
        .spacing(10);
        
        // Sound effects volume slider (only show if sound effects are enabled)
        let sfx_volume_slider = if settings.sound_effects_enabled {
            slider(0.0..=1.0, settings.sfx_volume, |value| {
                Message::SettingsChanged(SettingsMessage::SoundEffectsVolumeChanged(value))
            })
            .step(0.1)
            .width(Length::Fill)
        } else {
            slider(0.0..=1.0, 0.0, |_| Message::SettingsChanged(SettingsMessage::SoundEffectsVolumeChanged(0.0)))
                .step(0.1)
                .width(Length::Fill)
        };
        
        let sfx_volume_text = text(format!("Sound Effects Volume: {:.1}", settings.sfx_volume * 10.0))
            .size(16);

        // Game Settings
        let game_title = text("Game Settings")
            .size(24)
            .width(Length::Fill);
            
        // Difficulty level selector
        let difficulties = vec![Difficulty::Easy, Difficulty::Medium, Difficulty::Hard];
        
        let difficulty_selector = pick_list(
            difficulties,
            Some(settings.difficulty),
            |difficulty| Message::SettingsChanged(SettingsMessage::DifficultyChanged(difficulty))
        )
        .width(Length::Fixed(150.0));
        
        let difficulty_text = text("Game Difficulty:")
            .size(18);
            
        let difficulty_row = row![
            difficulty_text,
            difficulty_selector
        ]
        .spacing(10)
        .align_items(Alignment::Center);
        
        // Animations toggle
        let animations_checkbox = checkbox(
            "Enable Animations",
            settings.enable_animations,
            |checked| Message::SettingsChanged(SettingsMessage::AnimationsToggled(checked))
        )
        .size(18)
        .spacing(10);

        // Action buttons
        let button_row = row![
            button(text("Save Settings"))
                .on_press(Message::SettingsChanged(SettingsMessage::SaveSettings))
                .style(iced::theme::Button::Primary),
            button(text("Return to Menu"))
                .on_press(Message::SettingsChanged(SettingsMessage::Cancel))
                .style(iced::theme::Button::Secondary)
        ]
        .spacing(20)
        .width(Length::Fill)
        .align_items(Alignment::Center);

        // Player Settings
        let player_title = text("Player Settings")
            .size(24)
            .width(Length::Fill);
            
        // Player 1 name input
        let player1_name_text = text("Player 1 Name:")
            .size(18);
            
        let player1_name_input = text_input("Enter player 1 name", &settings.player1_name)
            .on_input(|name| Message::SettingsChanged(SettingsMessage::Player1NameChanged(name)))
            .padding(5)
            .width(Length::Fixed(200.0));
        
        let player1_row = row![
            player1_name_text,
            player1_name_input
        ]
        .spacing(10)
        .align_items(Alignment::Center);
        
        // Player 2 name input
        let player2_name_text = text("Player 2 Name:")
            .size(18);
            
        let player2_name_input = text_input("Enter player 2 name", &settings.player2_name)
            .on_input(|name| Message::SettingsChanged(SettingsMessage::Player2NameChanged(name)))
            .padding(5)
            .width(Length::Fixed(200.0));
        
        let player2_row = row![
            player2_name_text,
            player2_name_input
        ]
        .spacing(10)
        .align_items(Alignment::Center);
        
        // Combine all elements into the settings view
        let content = column![
            title,
            horizontal_rule(1),
            audio_title,
            music_checkbox,
            music_volume_text,
            music_volume_slider,
            sfx_checkbox,
            sfx_volume_text,
            sfx_volume_slider,
            horizontal_rule(1),
            game_title,
            difficulty_row,
            animations_checkbox,
            horizontal_rule(1),
            player_title,
            player1_row,
            player2_row,
            horizontal_rule(1),
            button_row
        ]
        .spacing(15)
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