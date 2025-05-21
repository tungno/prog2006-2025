use iced::{Color, Background, Theme};
use iced::widget::{button, container, text};

/// Style for primary buttons
pub struct PrimaryButton;

impl button::StyleSheet for PrimaryButton {
    type Style = Theme;

    fn active(&self, _style: &Self::Style) -> button::Appearance {
        button::Appearance {
            background: Some(Background::Color(Color::from_rgb(0.2, 0.5, 0.8))),
            border_radius: 4.0.into(),
            border_width: 1.0,
            border_color: Color::from_rgb(0.1, 0.4, 0.7),
            text_color: Color::WHITE,
            ..Default::default()
        }
    }

    fn hovered(&self, style: &Self::Style) -> button::Appearance {
        let active = self.active(style);

        button::Appearance {
            background: Some(Background::Color(Color::from_rgb(0.3, 0.6, 0.9))),
            ..active
        }
    }

    fn pressed(&self, style: &Self::Style) -> button::Appearance {
        let active = self.active(style);

        button::Appearance {
            background: Some(Background::Color(Color::from_rgb(0.1, 0.4, 0.7))),
            ..active
        }
    }
}

/// Style for secondary buttons
pub struct SecondaryButton;

impl button::StyleSheet for SecondaryButton {
    type Style = Theme;

    fn active(&self, _style: &Self::Style) -> button::Appearance {
        button::Appearance {
            background: Some(Background::Color(Color::from_rgb(0.8, 0.8, 0.8))),
            border_radius: 4.0.into(),
            border_width: 1.0,
            border_color: Color::from_rgb(0.7, 0.7, 0.7),
            text_color: Color::BLACK,
            ..Default::default()
        }
    }

    fn hovered(&self, style: &Self::Style) -> button::Appearance {
        let active = self.active(style);

        button::Appearance {
            background: Some(Background::Color(Color::from_rgb(0.9, 0.9, 0.9))),
            ..active
        }
    }

    fn pressed(&self, style: &Self::Style) -> button::Appearance {
        let active = self.active(style);

        button::Appearance {
            background: Some(Background::Color(Color::from_rgb(0.7, 0.7, 0.7))),
            ..active
        }
    }
}

/// Style for destructive buttons (delete, clear, etc.)
pub struct DestructiveButton;

impl button::StyleSheet for DestructiveButton {
    type Style = Theme;

    fn active(&self, _style: &Self::Style) -> button::Appearance {
        button::Appearance {
            background: Some(Background::Color(Color::from_rgb(0.8, 0.2, 0.2))),
            border_radius: 4.0.into(),
            border_width: 1.0,
            border_color: Color::from_rgb(0.7, 0.1, 0.1),
            text_color: Color::WHITE,
            ..Default::default()
        }
    }

    fn hovered(&self, style: &Self::Style) -> button::Appearance {
        let active = self.active(style);

        button::Appearance {
            background: Some(Background::Color(Color::from_rgb(0.9, 0.3, 0.3))),
            ..active
        }
    }

    fn pressed(&self, style: &Self::Style) -> button::Appearance {
        let active = self.active(style);

        button::Appearance {
            background: Some(Background::Color(Color::from_rgb(0.7, 0.1, 0.1))),
            ..active
        }
    }
}

/// Style for containers with borders
pub struct BoxedContainer;

impl container::StyleSheet for BoxedContainer {
    type Style = Theme;

    fn appearance(&self, _style: &Self::Style) -> container::Appearance {
        container::Appearance {
            background: Some(Background::Color(Color::from_rgb(0.95, 0.95, 0.95))),
            border_radius: 5.0.into(),
            border_width: 1.0,
            border_color: Color::from_rgb(0.8, 0.8, 0.8),
            ..Default::default()
        }
    }
}

/// Style for titles
pub struct Title;

impl text::StyleSheet for Title {
    type Style = Theme;

    fn appearance(&self, _style: Theme) -> text::Appearance {
        text::Appearance {
            color: Some(Color::from_rgb(0.2, 0.2, 0.2)),
        }
    }
}

/// Style for subtitles
pub struct Subtitle;

impl text::StyleSheet for Subtitle {
    type Style = Theme;

    fn appearance(&self, _style: Theme) -> text::Appearance {
        text::Appearance {
            color: Some(Color::from_rgb(0.4, 0.4, 0.4)),
        }
    }
}

/// Style for colored boxes in the UI
pub struct ColorBox(pub Color);

impl container::StyleSheet for ColorBox {
    type Style = Theme;

    fn appearance(&self, _style: &Self::Style) -> container::Appearance {
        container::Appearance {
            background: Some(Background::Color(self.0)),
            border_radius: 4.0.into(),
            border_width: 1.0,
            border_color: Color::BLACK,
            ..Default::default()
        }
    }
}

/// Style for selected items
pub struct Selected;

impl container::StyleSheet for Selected {
    type Style = Theme;

    fn appearance(&self, _style: &Self::Style) -> container::Appearance {
        container::Appearance {
            background: Some(Background::Color(Color::from_rgb(0.85, 0.85, 1.0))),
            border_radius: 4.0.into(),
            border_width: 1.0,
            border_color: Color::from_rgb(0.6, 0.6, 0.9),
            ..Default::default()
        }
    }
}

/// Style for day/night indicators
pub struct DayNightIndicator(pub bool); // true for day, false for night

impl container::StyleSheet for DayNightIndicator {
    type Style = Theme;

    fn appearance(&self, _style: &Self::Style) -> container::Appearance {
        container::Appearance {
            background: Some(Background::Color(
                if self.0 {
                    // Day - yellow
                    Color::from_rgb(1.0, 0.8, 0.0)
                } else {
                    // Night - dark blue
                    Color::from_rgb(0.1, 0.1, 0.4)
                }
            )),
            border_radius: 15.0.into(), // Circle
            border_width: 1.0,
            border_color: Color::BLACK,
            ..Default::default()
        }
    }
}