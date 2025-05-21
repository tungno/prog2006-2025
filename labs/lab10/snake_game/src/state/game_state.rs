/// Represents the possible modes of gameplay
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum GameMode {
    /// Single player mode - one snake, controlled by one player
    SinglePlayer,
    /// Multiplayer mode - two snakes, each controlled by a different player
    MultiPlayer,
}

/// Represents the current state of the game
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum GameState {
    /// Main menu state, where the player selects a game mode
    Menu,
    /// Settings screen
    Settings,
    /// High scores screen
    HighScores,
    /// Actively playing the game
    Playing {
        /// Which mode is being played (SinglePlayer or MultiPlayer)
        mode: GameMode,
        /// Whether the game is currently paused
        paused: bool,
    },
    /// Game over state, shown after a player loses
    GameOver,
    /// High score entry screen
    HighScoreEntry,
}

/// Implementation of game state
impl GameState {
    /// Creates a new default game state (Menu)
    pub fn new() -> Self {
        GameState::Menu
    }
    
    /// Determines if the game is currently in a playing state
    pub fn is_playing(&self) -> bool {
        matches!(self, GameState::Playing { .. })
    }
    
    /// Determines if the game is currently paused
    pub fn is_paused(&self) -> bool {
        match self {
            GameState::Playing { paused, .. } => *paused,
            _ => false,
        }
    }
    
    /// Gets the current game mode if in a playing state
    pub fn mode(&self) -> Option<GameMode> {
        match self {
            GameState::Playing { mode, .. } => Some(*mode),
            _ => None,
        }
    }
}