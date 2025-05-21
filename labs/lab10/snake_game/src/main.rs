// In src/main.rs
use iced::{Application, Command, Element, Settings, Theme};
use iced::executor;
use iced::window;
use iced::keyboard::KeyCode;
use std::time::{Duration, Instant};

mod logic;
mod state;
mod ui;
mod utils;
mod clear_high_scores;

use state::game_state::{GameMode, GameState};
use state::single_player_state::SinglePlayerState;
use state::multi_player_state::MultiPlayerState;
use ui::menu::Menu;
use ui::game_view::GameView;
use ui::game_over::GameOver;
use ui::settings::{Settings as SettingsScreen, SettingsMessage};
use ui::high_scores::HighScores;
use utils::helpers::{key_to_direction_p1, key_to_direction_p2};
use utils::storage::{GameSettings, get_storage, init_storage};
use utils::audio::{AudioManager, SoundEffect, play_sound, play_background_music, stop_background_music};

// Game update speed settings based on difficulty
const GAME_UPDATE_SPEED_EASY: u64 = 200;
const GAME_UPDATE_SPEED_MEDIUM: u64 = 150;
const GAME_UPDATE_SPEED_HARD: u64 = 100;

/// Main application structure that holds the game state
pub struct SnakeGame {
    /// Current state of the game (Menu, Settings, HighScores, Playing, GameOver)
    state: GameState,
    /// State for single player mode
    single_player_state: SinglePlayerState,
    /// State for multi-player mode
    multi_player_state: MultiPlayerState,
    /// Last score achieved (for game over screen)
    last_score: u32,
    /// Winner of the last game (for multiplayer game over)
    winner: Option<u8>,
    /// Last update time for controlling game speed
    last_update: Instant,
    /// Game settings
    settings: GameSettings,
    /// Player name for high scores
    player_name: String,
    /// Current game update speed in milliseconds (set based on difficulty)
    game_speed: u64,
}

/// Messages that can be sent to update the application state
#[derive(Debug, Clone)]
pub enum Message {
    /// Start a single player game
    StartSinglePlayer,
    /// Start a multiplayer game
    StartMultiPlayer,
    /// Show the settings screen
    ShowSettings,
    /// Show the high scores screen
    ShowHighScores,
    /// Return to the main menu
    ReturnToMenu,
    /// Update tick for game logic
    Tick,
    /// Key press event
    KeyPress(KeyCode),
    /// Game over event
    GameOver,
    /// Play again with the same mode
    PlayAgain,
    /// Settings changed
    SettingsChanged(SettingsMessage),
    /// Player name changed (for high scores)
    PlayerNameChanged(String),
    /// Submit high score
    SubmitHighScore,
    /// Clear all high scores
    ClearHighScores,
}

impl Application for SnakeGame {
    type Executor = executor::Default;
    type Message = Message;
    type Theme = Theme;
    type Flags = ();

    fn new(_flags: ()) -> (Self, Command<Message>) {
        println!("Initializing Snake Game");
        
        // Initialize storage
        let storage_initialized = init_storage();
        if !storage_initialized {
            println!("Warning: Failed to initialize storage, settings and high scores will not be saved");
        }
        
        // Initialize audio system
        let audio_initialized = AudioManager::init();
        if !audio_initialized {
            println!("Warning: Failed to initialize audio system, sounds will not be played");
        }
        
        // Load settings from storage or use defaults
        let settings = if let Some(storage) = get_storage() {
            storage.load_settings()
        } else {
            GameSettings::default()
        };
        
        // Set game speed based on difficulty
        let game_speed = match settings.difficulty {
            utils::storage::Difficulty::Easy => GAME_UPDATE_SPEED_EASY,
            utils::storage::Difficulty::Medium => GAME_UPDATE_SPEED_MEDIUM,
            utils::storage::Difficulty::Hard => GAME_UPDATE_SPEED_HARD,
        };
        
        // Play background music if enabled
        if settings.music_enabled {
            play_background_music("background.wav");
        }
        
        // Clone player1_name before moving settings
        let player_name = settings.player1_name.clone();
        
        (
            Self {
                state: GameState::Menu,
                single_player_state: SinglePlayerState::new(),
                multi_player_state: MultiPlayerState::new(),
                last_score: 0,
                winner: None,
                last_update: Instant::now(),
                settings,
                player_name,
                game_speed,
            },
            Command::none(),
        )
    }

    fn title(&self) -> String {
        String::from("Snake Game")
    }

    fn update(&mut self, message: Message) -> Command<Message> {
        match message {
            Message::StartSinglePlayer => {
                println!("Starting single player game with speed: {}ms", self.game_speed);
                // Reset single player state for a new game
                self.single_player_state = SinglePlayerState::new();
                self.state = GameState::Playing {
                    mode: GameMode::SinglePlayer,
                    paused: false,
                };
                
                self.last_update = Instant::now();
                
                // Play sound effect
                play_sound(SoundEffect::MenuSelect);
                
                // Start game timer with configured speed
                Command::perform(
                    async { std::time::Duration::from_millis(50) }, // Fast initial tick
                    |_| Message::Tick,
                )
            }
            Message::StartMultiPlayer => {
                println!("Starting multiplayer game with speed: {}ms", self.game_speed);
                // Reset multi-player state for a new game
                self.multi_player_state = MultiPlayerState::new();
                self.state = GameState::Playing {
                    mode: GameMode::MultiPlayer,
                    paused: false,
                };
                
                self.last_update = Instant::now();
                
                // Play sound effect
                play_sound(SoundEffect::MenuSelect);
                
                // Start game timer with configured speed
                Command::perform(
                    async { std::time::Duration::from_millis(50) }, // Fast initial tick
                    |_| Message::Tick,
                )
            }
            Message::ShowSettings => {
                play_sound(SoundEffect::MenuSelect);
                self.state = GameState::Settings;
                Command::none()
            }
            Message::ShowHighScores => {
                play_sound(SoundEffect::MenuSelect);
                self.state = GameState::HighScores;
                Command::none()
            }
            Message::ReturnToMenu => {
                play_sound(SoundEffect::MenuSelect);
                self.state = GameState::Menu;
                Command::none()
            }
            Message::Tick => {
                if let GameState::Playing { mode, paused } = &self.state {
                    if !paused {
                        let now = Instant::now();
                        let elapsed = now.duration_since(self.last_update);
                        
                        // Only update game state if enough time has passed
                        if elapsed >= Duration::from_millis(self.game_speed) {
                            self.last_update = now;
                            
                            // Update game state based on mode
                            match mode {
                                GameMode::SinglePlayer => {
                                    // Make sure food exists
                                    if self.single_player_state.food.position.x < 0 || 
                                       self.single_player_state.food.position.y < 0 {
                                        println!("Food not found! Generating new food.");
                                        self.single_player_state.generate_new_food();
                                    }
                                    
                                    // Check if snake ate food before updating
                                    let ate_food = self.single_player_state.snake.has_eaten(&self.single_player_state.food);
                                    
                                    // Update single player game state
                                    if !self.single_player_state.update() {
                                        println!("Single player game over! Final score: {}", self.single_player_state.score);
                                        // Play collision sound first
                                        play_sound(SoundEffect::Collision);
                                        
                                        // Then play game over sound
                                        play_sound(SoundEffect::GameOver);
                                        
                                        // Save high score if applicable
                                        if let Some(storage) = get_storage() {
                                            if self.single_player_state.score > 0 {
                                                let _ = storage.add_high_score(&self.player_name, self.single_player_state.score);
                                            }
                                        }
                                        
                                        // Game over
                                        self.last_score = self.single_player_state.score;
                                        return Command::perform(
                                            async { },
                                            |_| Message::GameOver,
                                        );
                                    }
                                    
                                    // Play eat sound if snake ate food
                                    if ate_food {
                                        println!("SNAKE ATE FOOD - PLAYING EAT SOUND");
                                        play_sound(SoundEffect::Eat);
                                    }
                                }
                                GameMode::MultiPlayer => {
                                    // Make sure food exists
                                    if self.multi_player_state.food.position.x < 0 || 
                                       self.multi_player_state.food.position.y < 0 {
                                        println!("Food not found! Generating new food in multiplayer.");
                                        self.multi_player_state.generate_new_food();
                                    }
                                    
                                    // Check if snakes ate food before updating
                                    let snake1_ate = self.multi_player_state.snake1.has_eaten(&self.multi_player_state.food);
                                    let snake2_ate = self.multi_player_state.snake2.has_eaten(&self.multi_player_state.food);
                                    
                                    // Check if the game is over - either through a previous update
                                    // or it will be after this update
                                    if self.multi_player_state.game_over {
                                        println!("Multiplayer game already over");
                                        self.winner = self.multi_player_state.winner;
                                        
                                        // Game over sound
                                        play_sound(SoundEffect::GameOver);
                                        
                                        return Command::perform(
                                            async { },
                                            |_| Message::GameOver,
                                        );
                                    }
                                    
                                    // Store alive states before update to detect collisions
                                    let was_alive1 = self.multi_player_state.alive1;
                                    let was_alive2 = self.multi_player_state.alive2;
                                    
                                    // Update multi-player game state
                                    let update_result = self.multi_player_state.update();
                                    
                                    // Check if any player just died and play collision sound
                                    if was_alive1 && !self.multi_player_state.alive1 {
                                        play_sound(SoundEffect::Collision);
                                    }
                                    if was_alive2 && !self.multi_player_state.alive2 {
                                        play_sound(SoundEffect::Collision);
                                    }
                                    
                                    if !update_result {
                                        println!("Multiplayer game over");
                                        // Game over, capture winner before state change
                                        self.winner = self.multi_player_state.winner;
                                        
                                        // Game over sound
                                        play_sound(SoundEffect::GameOver);
                                        
                                        // Save high score for the winner if applicable
                                        if let Some(storage) = get_storage() {
                                            // Store highest score between both players
                                            let highest_score = std::cmp::max(
                                                self.multi_player_state.score1,
                                                self.multi_player_state.score2
                                            );
                                            
                                            if highest_score > 0 {
                                                let player_name = match self.winner {
                                                    Some(1) => &self.settings.player1_name,
                                                    Some(2) => &self.settings.player2_name,
                                                    _ => "Players",
                                                };
                                                
                                                let _ = storage.add_high_score(player_name, highest_score);
                                            }
                                        }
                                        
                                        return Command::perform(
                                            async { },
                                            |_| Message::GameOver,
                                        );
                                    }
                                    
                                    // Play eat sound if either snake ate food
                                    if snake1_ate || snake2_ate {
                                        println!("MULTIPLAYER - SNAKE ATE FOOD - PLAYING EAT SOUND");
                                        play_sound(SoundEffect::Eat);
                                    }
                                }
                            }
                        }
                        
                        // Schedule next tick (quickly, but we'll only update the game on certain ticks)
                        return Command::perform(
                            async { std::time::Duration::from_millis(16) }, // ~60fps for smooth UI
                            |_| Message::Tick,
                        );
                    }
                }
                Command::none()
            }
            Message::KeyPress(key_code) => {
                if let GameState::Playing { mode, paused } = &mut self.state {
                    match key_code {
                        KeyCode::P => {
                            *paused = !*paused;
                            println!("Game paused: {}", *paused);
                            if !*paused {
                                self.last_update = Instant::now();
                                return Command::perform(
                                    async { std::time::Duration::from_millis(16) },
                                    |_| Message::Tick,
                                );
                            }
                        }
                        // Handle movement keys
                        _ => {
                            if !*paused {
                                match mode {
                                    GameMode::SinglePlayer => {
                                        // Only process inputs if game is still active
                                        if !self.single_player_state.game_over {
                                            // Handle single player controls (WASD)
                                            if let Some(direction) = key_to_direction_p1(key_code) {
                                                println!("Player 1 changing direction");
                                                self.single_player_state.snake.change_direction(direction);
                                            }
                                        }
                                    }
                                    GameMode::MultiPlayer => {
                                        // Only process inputs if game is still active
                                        if !self.multi_player_state.game_over {
                                            // Handle multi-player controls (WASD for P1, Arrows for P2)
                                            if let Some(direction) = key_to_direction_p1(key_code) {
                                                if self.multi_player_state.alive1 {
                                                    println!("Player 1 changing direction in multiplayer");
                                                    self.multi_player_state.snake1.change_direction(direction);
                                                }
                                            }
                                            
                                            if let Some(direction) = key_to_direction_p2(key_code) {
                                                if self.multi_player_state.alive2 {
                                                    println!("Player 2 changing direction in multiplayer");
                                                    self.multi_player_state.snake2.change_direction(direction);
                                                }
                                            }
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
                Command::none()
            }
            Message::GameOver => {
                println!("Game over event received");
                self.state = GameState::GameOver;
                Command::none()
            }
            Message::PlayAgain => {
                if let GameState::GameOver = self.state {
                    // Play menu select sound
                    play_sound(SoundEffect::MenuSelect);
                    
                    // Determine which mode to restart
                    if self.winner.is_some() {
                        // Was multiplayer
                        println!("Restarting multiplayer game");
                        return self.update(Message::StartMultiPlayer);
                    } else {
                        // Was single player
                        println!("Restarting single player game");
                        return self.update(Message::StartSinglePlayer);
                    }
                }
                Command::none()
            }
            Message::SettingsChanged(settings_msg) => {
                match settings_msg {
                    SettingsMessage::MusicToggled(enabled) => {
                        self.settings.music_enabled = enabled;
                        
                        // Update audio playback based on setting
                        if enabled {
                            play_background_music("assets/audio/background.wav");
                        } else {
                            stop_background_music();
                        }
                    }
                    SettingsMessage::SoundEffectsToggled(enabled) => {
                        self.settings.sound_effects_enabled = enabled;
                        
                        // Play test sound if enabling
                        if enabled {
                            play_sound(SoundEffect::MenuSelect);
                        }
                    }
                    SettingsMessage::MusicVolumeChanged(volume) => {
                        self.settings.music_volume = volume;
                        
                        // Update audio manager
                        utils::audio::update_audio_settings(self.settings.clone());
                    }
                    SettingsMessage::SoundEffectsVolumeChanged(volume) => {
                        self.settings.sfx_volume = volume;
                        
                        // Update audio manager
                        utils::audio::update_audio_settings(self.settings.clone());
                        
                        // Play test sound
                        if self.settings.sound_effects_enabled {
                            play_sound(SoundEffect::MenuSelect);
                        }
                    }
                    SettingsMessage::DifficultyChanged(difficulty) => {
                        self.settings.difficulty = difficulty;
                        
                        // Update game speed based on difficulty
                        self.game_speed = match difficulty {
                            utils::storage::Difficulty::Easy => GAME_UPDATE_SPEED_EASY,
                            utils::storage::Difficulty::Medium => GAME_UPDATE_SPEED_MEDIUM,
                            utils::storage::Difficulty::Hard => GAME_UPDATE_SPEED_HARD,
                        };
                    }
                    SettingsMessage::AnimationsToggled(enabled) => {
                        self.settings.enable_animations = enabled;
                    }
                    SettingsMessage::Player1NameChanged(name) => {
                        self.settings.player1_name = name;
                    }
                    SettingsMessage::Player2NameChanged(name) => {
                        self.settings.player2_name = name;
                    }
                    SettingsMessage::SaveSettings => {
                        // Save settings to storage
                        if let Some(storage) = get_storage() {
                            match storage.save_settings(&self.settings) {
                                Ok(_) => println!("Settings saved successfully"),
                                Err(e) => println!("Failed to save settings: {}", e),
                            }
                        }
                        
                        // Play confirmation sound
                        play_sound(SoundEffect::MenuSelect);
                        
                        // Return to menu
                        self.state = GameState::Menu;
                    }
                    SettingsMessage::Cancel => {
                        // Reload settings from storage
                        if let Some(storage) = get_storage() {
                            self.settings = storage.load_settings();
                        }
                        
                        // Play cancel sound
                        play_sound(SoundEffect::MenuSelect);
                        
                        // Return to menu
                        self.state = GameState::Menu;
                    }
                }
                Command::none()
            }
            Message::PlayerNameChanged(name) => {
                self.player_name = name;
                Command::none()
            }
            Message::SubmitHighScore => {
                // Save high score
                if let Some(storage) = get_storage() {
                    if let GameState::HighScoreEntry = self.state {
                        let _ = storage.add_high_score(&self.player_name, self.last_score);
                    }
                }
                
                // Navigate to high scores view
                self.state = GameState::HighScores;
                Command::none()
            }
            Message::ClearHighScores => {
                // Clear all high scores
                let cleared = clear_high_scores::clear_high_scores();
                println!("High scores cleared: {}", cleared);
                
                // Stay on high scores screen with cleared data
                self.state = GameState::HighScores;
                Command::none()
            }
        }
    }

    fn subscription(&self) -> iced::Subscription<Message> {
        // Subscribe to keyboard events
        iced::subscription::events_with(|event, _| {
            if let iced::Event::Keyboard(keyboard_event) = event {
                if let iced::keyboard::Event::KeyPressed { key_code, .. } = keyboard_event {
                    return Some(Message::KeyPress(key_code));
                }
            }
            None
        })
    }

    fn view(&self) -> Element<Message> {
        match &self.state {
            GameState::Menu => Menu::view(),
            GameState::Settings => SettingsScreen::view(&self.settings),
            GameState::HighScores => HighScores::view(),
            GameState::Playing { mode, paused } => {
                match mode {
                    GameMode::SinglePlayer => {
                        GameView::view_single_player(&self.single_player_state, *paused)
                    }
                    GameMode::MultiPlayer => {
                        GameView::view_multi_player(&self.multi_player_state, *paused)
                    }
                }
            }
            GameState::GameOver => {
                println!("Rendering game over screen, winner: {:?}", self.winner);
                if self.winner.is_some() {
                    // Multi-player game over
                    GameOver::view_multi_player(
                        self.multi_player_state.score1,
                        self.multi_player_state.score2,
                        self.winner
                    )
                } else {
                    // Single player game over
                    GameOver::view_single_player(
                        self.last_score,
                        self.single_player_state.high_score
                    )
                }
            }
            GameState::HighScoreEntry => {
                // High score entry UI would be implemented here
                // Just show high scores view
                HighScores::view()
            }
        }
    }
}

fn main() -> iced::Result {
    println!("Starting Snake Game application");
    
    // Use a more appropriate window size that works well on different displays
    SnakeGame::run(Settings {
        window: window::Settings {
            size: (800, 650),  // Slightly taller to accommodate UI elements
            min_size: Some((600, 500)),  // Ensure minimum usable size
            resizable: true,  // Allow resizing for better usability
            decorations: true,  // Use window decorations
            ..window::Settings::default()
        },
        antialiasing: true,  // Enable antialiasing for smoother graphics
        ..Settings::default()
    })
}