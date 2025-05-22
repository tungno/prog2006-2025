# Snake Game

A sophisticated Snake game implementation using Rust and the Iced framework, featuring both single-player and two-player modes with custom settings, audio, and high score tracking.

![Snake Game Screenshot](https://i.imgur.com/mDeCv3l.png)

## Features

- **Single-Player Mode**: Classic snake gameplay with score tracking
- **Two-Player Mode**: Competitive gameplay on the same keyboard
- **Customizable Settings**: Adjust difficulty, volume, animations, and player names
- **High Score System**: Persistent leaderboard across game sessions
- **Audio System**: Background music and sound effects for game events
- **Smooth Controls**: Responsive keyboard input for fluid gameplay
- **Pause Functionality**: Pause the game at any time with the 'P' key
- **Grid Wrapping**: Snake passes through screen edges and appears from the opposite side

## Prerequisites

- Rust (latest stable version recommended)
- Cargo package manager

## Installation & Running

1. Clone the repository or navigate to the project directory
2. Build and run the game:

```bash
# Run the game
cargo run --bin snake_game

# For better performance, use release mode
cargo run --bin snake_game --release
```

## Controls

### Single-Player Mode
- **W**: Move Up
- **A**: Move Left
- **S**: Move Down
- **D**: Move Right
- **P**: Pause/Resume game
- **ESC**: Return to main menu (during gameplay)

### Two-Player Mode
- **Player 1**:
  - **W**: Move Up
  - **A**: Move Left
  - **S**: Move Down
  - **D**: Move Right
- **Player 2**:
  - **↑**: Move Up
  - **←**: Move Left
  - **↓**: Move Down
  - **→**: Move Right
- **P**: Pause/Resume game
- **ESC**: Return to main menu (during gameplay)

### Menu Navigation
- Use mouse clicks to navigate menus and select game modes
- Adjust settings in the settings menu

## Implementation Details

### Architecture

The game follows a modular architecture organized into several key components:

- **State Management**: Clean separation between state and view using an Elm-like architecture
  - `state/game_state.rs`: Core game state enum with different game modes
  - `state/single_player_state.rs` & `state/multi_player_state.rs`: Game mode-specific states
  
- **Game Logic**: Core game mechanics are implemented in the `logic/` directory
  - `snake.rs`: Snake movement, growth, and collision detection
  - `food.rs`: Food generation and placement
  - `collision.rs`: Collision detection utilities
  - `grid.rs`: Grid system implementation

- **User Interface**: The `ui/` directory handles all rendering and user interaction
  - `menu.rs`: Main menu screen
  - `game_view.rs`: Active gameplay rendering
  - `game_over.rs`: Game over screen
  - `settings.rs`: Settings configuration screen
  - `high_scores.rs`: High score display

- **Utilities**: Support functionalities in `utils/` directory
  - `audio.rs`: Threaded audio system with message passing
  - `storage.rs`: Persistent storage for settings and high scores
  - `constants.rs`: Game configuration constants
  - `helpers.rs`: Helper utilities

### Technical Highlights

#### Snake Movement and Collision
The snake is represented as a vector of positions, with movement handled by adding a new head position and removing the tail (unless growing). Collision detection checks for intersections with walls, the snake's own body, or other snakes.

#### State Management
The game uses Rust's powerful enum pattern for clean state transitions:
- `GameState`: Tracks if the game is in Menu, Settings, Playing, or GameOver state
- `GameMode`: Identifies if playing mode is SinglePlayer or MultiPlayer

#### Audio System
The game features a sophisticated audio system:
- Dedicated audio thread to prevent blocking the main game loop
- Message passing for non-blocking audio operations
- Volume control for both music and sound effects
- Sound caching to prevent audio spam
- Background music with infinite looping

#### Data Persistence
Game settings and high scores are saved between sessions:
- Cross-platform storage locations using the `directories` crate
- JSON serialization with `serde` for settings and high scores
- High score entries include player name, score, and date/time

#### Reactive UI
The UI is built using Iced's reactive architecture:
- Canvas-based game rendering for efficient graphics
- Responsive layout that adapts to window size
- Clean separation between UI and game logic

## Future Roadmap

### Mobile and Desktop Cross-Platform Development
- **Mobile Adaptation**: Port the game to iOS and Android platforms using cross-platform Rust frameworks
- **Touch Controls**: Implement swipe gestures for mobile controls
- **Native Builds**: Create native packages for macOS, Windows, and Linux with installers

### Social and Multiplayer Features
- **Online Multiplayer**: Add network play capabilities for remote opponents
- **Friend Challenges**: Challenge friends to beat your high scores
- **Leaderboards**: Global leaderboards with competitive rankings
- **Achievements**: Unlock achievements for special accomplishments

### Growth and Scale
- **Target User Base**: Scale to support 1 million active users
- **Cloud Infrastructure**: Implement backend services for user accounts and leaderboards
- **Analytics**: Add telemetry to understand player behavior
- **Localization**: Support multiple languages

### Enhanced Gameplay
- **Power-ups**: Special abilities like invincibility, speed boost, or score multipliers
- **Level Progression**: Increasing difficulty with advancing levels
- **Game Modes**: Time attack, survival, and obstacle modes
- **Customization**: Snake skins, game themes, and custom grid sizes

### Revenue Model
- **In-App Purchases**: Cosmetic items and power-ups
- **Premium Version**: Ad-free experience with additional features
- **Cross-Promotion**: Partner with other games for cross-promotion

## Built With

- [Rust](https://www.rust-lang.org/) - Programming language
- [Iced](https://github.com/iced-rs/iced) - GUI framework
- [Rodio](https://github.com/RustAudio/rodio) - Audio playback
- [Serde](https://github.com/serde-rs/serde) - Serialization framework
- [Directories](https://github.com/dirs-dev/directories-rs) - Path management
- [Rand](https://github.com/rust-random/rand) - Random number generation
- [Chrono](https://github.com/chronotope/chrono) - Date and time library


## Acknowledgments

- Classic Snake game for inspiration
- Rust community for excellent libraries and documentation
- Iced framework for enabling reactive UI development in Rust