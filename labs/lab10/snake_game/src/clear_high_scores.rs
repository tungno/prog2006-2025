use crate::utils::storage::{get_storage, GameData, init_storage};

pub fn clear_high_scores() -> bool {
    // Initialize storage
    if !init_storage() {
        println!("Failed to initialize storage");
        return false;
    }
    
    // Get storage manager
    if let Some(storage) = get_storage() {
        // Create empty game data
        let empty_data = GameData::default();
        
        // Save empty data
        match storage.save_game_data(&empty_data) {
            Ok(_) => {
                println!("High scores cleared successfully");
                return true;
            },
            Err(e) => {
                println!("Failed to clear high scores: {}", e);
                return false;
            }
        }
    } else {
        println!("Failed to get storage manager");
        return false;
    }
}