use directories::ProjectDirs;
use serde::{Deserialize, Serialize};
use std::fmt;
use std::fs;
use std::path::PathBuf;

// Game settings structure to store user preferences
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct GameSettings {
    pub music_enabled: bool,
    pub sound_effects_enabled: bool,
    pub music_volume: f32,
    pub sfx_volume: f32,
    pub difficulty: Difficulty,
    pub enable_animations: bool,
    pub player1_name: String,
    pub player2_name: String,
}

// Game difficulty level
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum Difficulty {
    Easy,
    Medium,
    Hard,
}

impl fmt::Display for Difficulty {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Difficulty::Easy => write!(f, "Easy"),
            Difficulty::Medium => write!(f, "Medium"),
            Difficulty::Hard => write!(f, "Hard"),
        }
    }
}

impl Default for GameSettings {
    fn default() -> Self {
        Self {
            music_enabled: true,
            sound_effects_enabled: true,
            music_volume: 0.7,
            sfx_volume: 0.8,
            difficulty: Difficulty::Medium,
            enable_animations: true,
            player1_name: String::from("Player 1"),
            player2_name: String::from("Player 2"),
        }
    }
}

// Game data structure to store high scores
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct GameData {
    pub high_score: u32,
    pub high_scores: Vec<HighScoreEntry>,
}

impl Default for GameData {
    fn default() -> Self {
        Self {
            high_score: 0,
            high_scores: Vec::new(),
        }
    }
}

// Structure to represent a high score entry
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct HighScoreEntry {
    pub player_name: String,
    pub score: u32,
    pub date: String,
}

// Storage manager to handle saving and loading game data
pub struct StorageManager {
    data_dir: PathBuf,
}

impl StorageManager {
    // Initialize the storage manager
    pub fn new() -> Option<Self> {
        if let Some(proj_dirs) = ProjectDirs::from("com", "Snake", "SnakeGame") {
            let data_dir = proj_dirs.data_dir().to_path_buf();
            
            // Create directories if they don't exist
            if !data_dir.exists() {
                if let Err(e) = fs::create_dir_all(&data_dir) {
                    eprintln!("Failed to create data directory: {}", e);
                    return None;
                }
            }
            
            Some(Self { data_dir })
        } else {
            eprintln!("Could not find a valid home directory path");
            None
        }
    }
    
    // Save settings to disk
    pub fn save_settings(&self, settings: &GameSettings) -> Result<(), String> {
        let settings_path = self.data_dir.join("settings.json");
        let serialized = serde_json::to_string_pretty(settings)
            .map_err(|e| format!("Failed to serialize settings: {}", e))?;
            
        fs::write(&settings_path, serialized)
            .map_err(|e| format!("Failed to write settings file: {}", e))?;
            
        Ok(())
    }
    
    // Load settings from disk
    pub fn load_settings(&self) -> GameSettings {
        let settings_path = self.data_dir.join("settings.json");
        
        if !settings_path.exists() {
            return GameSettings::default();
        }
        
        match fs::read_to_string(&settings_path) {
            Ok(contents) => {
                match serde_json::from_str(&contents) {
                    Ok(settings) => settings,
                    Err(e) => {
                        eprintln!("Failed to parse settings file: {}", e);
                        GameSettings::default()
                    }
                }
            },
            Err(e) => {
                eprintln!("Failed to read settings file: {}", e);
                GameSettings::default()
            }
        }
    }
    
    // Save game data (high scores) to disk
    pub fn save_game_data(&self, game_data: &GameData) -> Result<(), String> {
        let data_path = self.data_dir.join("game_data.json");
        let serialized = serde_json::to_string_pretty(game_data)
            .map_err(|e| format!("Failed to serialize game data: {}", e))?;
            
        fs::write(&data_path, serialized)
            .map_err(|e| format!("Failed to write game data file: {}", e))?;
            
        Ok(())
    }
    
    // Load game data from disk
    pub fn load_game_data(&self) -> GameData {
        let data_path = self.data_dir.join("game_data.json");
        
        if !data_path.exists() {
            return GameData::default();
        }
        
        match fs::read_to_string(&data_path) {
            Ok(contents) => {
                match serde_json::from_str(&contents) {
                    Ok(data) => data,
                    Err(e) => {
                        eprintln!("Failed to parse game data file: {}", e);
                        GameData::default()
                    }
                }
            },
            Err(e) => {
                eprintln!("Failed to read game data file: {}", e);
                GameData::default()
            }
        }
    }
    
    // Add a new high score entry and save to disk
    pub fn add_high_score(&self, player_name: &str, score: u32) -> Result<(), String> {
        let mut game_data = self.load_game_data();
        
        // Update the all-time high score if needed
        if score > game_data.high_score {
            game_data.high_score = score;
        }
        
        // Create a new high score entry
        let date = chrono::Local::now().format("%Y-%m-%d %H:%M").to_string();
        let entry = HighScoreEntry {
            player_name: player_name.to_string(),
            score,
            date,
        };
        
        // Add the entry to the list and sort by score (descending)
        game_data.high_scores.push(entry);
        game_data.high_scores.sort_by(|a, b| b.score.cmp(&a.score));
        
        // Keep only the top 10 scores
        if game_data.high_scores.len() > 10 {
            game_data.high_scores.truncate(10);
        }
        
        // Save the updated game data
        self.save_game_data(&game_data)
    }
}

// Global storage manager instance for easy access throughout the app
use once_cell::sync::OnceCell;
static STORAGE_MANAGER: OnceCell<StorageManager> = OnceCell::new();

// Initialize the global storage manager
pub fn init_storage() -> bool {
    if STORAGE_MANAGER.get().is_some() {
        return true; // Already initialized
    }
    
    match StorageManager::new() {
        Some(manager) => STORAGE_MANAGER.set(manager).is_ok(),
        None => false,
    }
}

// Get a reference to the global storage manager
pub fn get_storage() -> Option<&'static StorageManager> {
    STORAGE_MANAGER.get()
}