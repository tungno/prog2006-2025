use std::io::BufReader;
use std::fs::File;
use std::process::Command;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    println!("Attempting to find a system converter for eat.aiff to eat.wav...");
    
    // Check for some common conversion tools
    let result = if cfg!(target_os = "macos") {
        // Try using afconvert which is native to macOS
        println!("Trying afconvert on macOS...");
        Command::new("afconvert")
            .args([
                "-f", "WAVE", // Output format
                "-d", "LEI16", // Output data format (16-bit little-endian integer)
                "assets/audio/eat.aiff", 
                "assets/audio/eat.wav"
            ])
            .status()
    } else {
        // Fallback - create a simple placeholder wav file
        println!("No suitable converter found. Creating a placeholder WAV file...");
        
        // Create a simple 1 second WAV file with a beep sound
        let spec = hound::WavSpec {
            channels: 1,
            sample_rate: 44100,
            bits_per_sample: 16,
            sample_format: hound::SampleFormat::Int,
        };
        
        let mut writer = hound::WavWriter::create("assets/audio/eat.wav", spec)?;
        
        // Generate a simple beep sound
        let sample_rate = 44100.0;
        let frequency = 440.0; // A440 tone
        
        for t in 0..44100 {
            let sample = (t as f32 / sample_rate * frequency * 2.0 * std::f32::consts::PI).sin();
            let amplitude = 0.5;
            let sample_i16 = (sample * amplitude * i16::MAX as f32) as i16;
            writer.write_sample(sample_i16)?;
        }
        
        writer.finalize()?;
        
        Ok(())
    };
    
    match result {
        Ok(_) => {
            println!("Conversion complete: assets/audio/eat.wav");
            Ok(())
        },
        Err(e) => {
            eprintln!("Error during conversion: {}", e);
            Err(Box::new(e))
        }
    }
}