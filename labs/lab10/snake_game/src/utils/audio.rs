// Optimized audio system with dedicated audio thread and message passing
// Uses a fire-and-forget approach for sound effects and persistent sink for music

use crate::utils::storage::{GameSettings, get_storage};
use rodio::{Decoder, OutputStream, Sink, Source};
use std::fs::File;
use std::io::BufReader;
use std::sync::mpsc;
use std::thread;
use std::time::{Duration, Instant};
use std::collections::HashMap;

// Audio effect types
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum SoundEffect {
    Eat,
    Collision,
    GameOver,
    MenuSelect,
    PowerUp,
}

impl SoundEffect {
    // Get the filename for a sound effect
    fn filename(&self) -> &'static str {
        match self {
            // Use the .wav files for all sounds for better compatibility
            SoundEffect::Eat => "eat_new.wav", // Using a working eat sound
            SoundEffect::Collision => "collision.wav",
            SoundEffect::GameOver => "collision.wav", // Temporarily use collision.wav for game over
            SoundEffect::MenuSelect => "menu_select.wav",
            SoundEffect::PowerUp => "power_up.wav",
        }
    }
}

// Messages that can be sent to the audio thread
enum AudioMessage {
    PlaySound(SoundEffect),
    PlayMusic(String),
    StopMusic,
    UpdateSettings(GameSettings),
    Shutdown,
}

// Global channel sender for audio messages
static mut AUDIO_SENDER: Option<mpsc::Sender<AudioMessage>> = None;

// Audio manager
pub struct AudioManager;

impl AudioManager {
    // Initialize the audio system by starting the audio thread
    pub fn init() -> bool {
        println!("Initializing audio system...");

        // Create a channel for sending messages to the audio thread
        let (sender, receiver) = mpsc::channel();

        // Store the sender globally so we can send messages from anywhere
        unsafe {
            AUDIO_SENDER = Some(sender);
        }

        // Start the audio thread
        thread::spawn(move || {
            // Create an audio output stream
            let (stream, stream_handle) = match OutputStream::try_default() {
                Ok((s, h)) => (s, h),
                Err(e) => {
                    eprintln!("Failed to create audio stream: {}", e);
                    return;
                }
            };

            // For background music
            let mut music_sink: Option<Sink> = None;
            
            // Load settings from storage
            let mut settings = if let Some(storage) = get_storage() {
                storage.load_settings()
            } else {
                GameSettings::default()
            };
            
            // Cache for decoded sound effects (optimization)
            let mut sound_cache: HashMap<SoundEffect, Instant> = HashMap::new();
            
            println!("Audio thread started. Settings: music={}, sfx={}", 
                settings.music_enabled, settings.sound_effects_enabled);

            // Process messages
            loop {
                match receiver.recv() {
                    Ok(message) => match message {
                        AudioMessage::PlaySound(effect) => {
                            if !settings.sound_effects_enabled {
                                continue;
                            }

                            // Rate limit sounds to avoid too many of the same sound at once
                            let now = Instant::now();
                            if let Some(last_played) = sound_cache.get(&effect) {
                                if now.duration_since(*last_played) < Duration::from_millis(100) {
                                    // Skip if the same sound was played very recently
                                    continue;
                                }
                            }
                            
                            // Update cache with current time
                            sound_cache.insert(effect, now);
                            
                            // Get the file path
                            let path = get_audio_path(effect.filename());
                            
                            // Open the audio file
                            match File::open(&path) {
                                Ok(file) => {
                                    let buf_reader = BufReader::new(file);
                                    
                                    // Decode the audio file
                                    println!("Attempting to play sound: {}", path);
                                    match Decoder::new(buf_reader) {
                                        Ok(source) => {
                                            println!("Successfully decoded sound: {}", path);
                                            // Apply volume adjustment
                                            let source = source.amplify(settings.sfx_volume);
                                            
                                            // Play the sound
                                            match stream_handle.play_raw(source.convert_samples()) {
                                                Ok(_) => println!("Successfully started playback for: {}", path),
                                                Err(e) => eprintln!("Failed to play sound {}: {}", path, e),
                                            }
                                        },
                                        Err(e) => eprintln!("Failed to decode audio file {}: {}", path, e),
                                    }
                                },
                                Err(e) => eprintln!("Failed to open audio file {}: {}", path, e),
                            }
                        },
                        AudioMessage::PlayMusic(filename) => {
                            if !settings.music_enabled {
                                continue;
                            }

                            // Stop current music if playing
                            if let Some(sink) = &music_sink {
                                sink.stop();
                            }
                            
                            // Get the file path
                            let path = get_audio_path(&filename);
                            
                            // Open the audio file
                            match File::open(&path) {
                                Ok(file) => {
                                    let buf_reader = BufReader::new(file);
                                    
                                    // Decode the audio file
                                    match Decoder::new(buf_reader) {
                                        Ok(source) => {
                                            // Create a new sink for the music
                                            match Sink::try_new(&stream_handle) {
                                                Ok(sink) => {
                                                    // Set volume
                                                    sink.set_volume(settings.music_volume);
                                                    
                                                    // Create a looping source
                                                    let source = source.repeat_infinite();
                                                    
                                                    // Play the music
                                                    sink.append(source);
                                                    
                                                    // Store the sink
                                                    music_sink = Some(sink);
                                                },
                                                Err(e) => eprintln!("Failed to create audio sink: {}", e),
                                            }
                                        },
                                        Err(e) => eprintln!("Failed to decode audio file {}: {}", path, e),
                                    }
                                },
                                Err(e) => eprintln!("Failed to open audio file {}: {}", path, e),
                            }
                        },
                        AudioMessage::StopMusic => {
                            // Stop current music if playing
                            if let Some(sink) = &music_sink {
                                sink.stop();
                            }
                            music_sink = None;
                        },
                        AudioMessage::UpdateSettings(new_settings) => {
                            let music_enabled_changed = settings.music_enabled != new_settings.music_enabled;
                            let music_volume_changed = settings.music_volume != new_settings.music_volume;
                            
                            // Update settings
                            settings = new_settings;
                            
                            // Update music if playing
                            if let Some(sink) = &music_sink {
                                if music_volume_changed {
                                    sink.set_volume(settings.music_volume);
                                }
                                
                                if !settings.music_enabled && music_enabled_changed {
                                    sink.pause();
                                } else if settings.music_enabled && music_enabled_changed {
                                    sink.play();
                                }
                            }
                        },
                        AudioMessage::Shutdown => {
                            // Stop music and shutdown
                            if let Some(sink) = &music_sink {
                                sink.stop();
                            }
                            break;
                        },
                    },
                    Err(_) => {
                        // Channel closed, exit thread
                        break;
                    }
                }
            }
            
            println!("Audio thread stopped.");
        });

        // Load settings from storage
        if let Some(storage) = get_storage() {
            let settings = storage.load_settings();
            
            // Play background music if enabled
            if settings.music_enabled {
                play_background_music("background.wav");
            }
        } else {
            // Play background music with default settings
            play_background_music("background.wav");
        }

        true
    }
}

// Helper function to send a message to the audio thread
fn send_audio_message(message: AudioMessage) {
    unsafe {
        if let Some(sender) = &AUDIO_SENDER {
            if sender.send(message).is_err() {
                // Silently fail - the audio thread may have shut down
            }
        }
    }
}

// Helper functions for easy audio playback
pub fn play_sound(effect: SoundEffect) {
    send_audio_message(AudioMessage::PlaySound(effect));
}

pub fn play_background_music(filename: &str) {
    // Fix path duplication by removing any "assets/audio/" prefix
    let clean_filename = filename.trim_start_matches("assets/audio/");
    send_audio_message(AudioMessage::PlayMusic(clean_filename.to_string()));
}

pub fn stop_background_music() {
    send_audio_message(AudioMessage::StopMusic);
}

pub fn update_audio_settings(settings: GameSettings) {
    send_audio_message(AudioMessage::UpdateSettings(settings));
}

// Helper function to get full audio path
fn get_audio_path(filename: &str) -> String {
    format!("assets/audio/{}", filename)
}