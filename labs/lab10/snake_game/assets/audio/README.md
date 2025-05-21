# Snake Game Audio System

This directory contains the audio files used in the Snake Game.

## Audio Files

- `background.wav` - Background music for the game
- `collision.wav` - Sound played when the snake collides with a wall or itself
- `eat.aiff` - Sound played when the snake eats food
- `game_over.aiff` - Sound played when the game ends
- `menu_select.wav` - Sound played when navigating through menus
- `power_up.wav` - Sound played when the snake collects a power-up

## Implementation Details

The audio system uses a dedicated audio thread with message passing:

1. **Dedicated Audio Thread**: All audio playback happens in a separate thread
2. **Message Queue**: Audio requests are sent through a message channel
3. **Fire-and-Forget**: Sound effects are played with a simple "fire-and-forget" approach
4. **Settings-Aware**: Respects audio settings (on/off and volume)
5. **Thread-Safe**: Uses message passing to avoid thread-safety issues

This approach avoids the thread-safety issues that can occur with the rodio audio library on some systems.

## Usage

To play a sound effect:
```rust
play_sound(SoundEffect::Eat);
```

To play background music:
```rust
play_background_music("background.wav");
```

To stop background music:
```rust
stop_background_music();
```

## Technical Details

1. When the audio system initializes, it creates a dedicated thread for audio playback
2. The main thread communicates with the audio thread through a message channel
3. The audio thread maintains its own stream, decoder, and sink objects
4. Sound effects are played immediately and discarded
5. Background music is played through a persistent sink that's retained
6. When settings change, the audio thread updates its state accordingly

## Troubleshooting

If audio doesn't play correctly:
1. Make sure the audio files exist in the assets/audio directory
2. Check that the file formats are recognized (.wav and .aiff are supported)
3. Ensure file permissions allow the app to read the audio files
4. Verify that volume is not set to zero or muted in settings
5. Check the console for any error messages related to audio playback