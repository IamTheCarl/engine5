pub const SAVE_DIRECTORY: &str = "worlds";
pub const CONFIG_DIRECTORY: &str = "config";

pub fn create_folders() {
    if let Err(error) = std::fs::create_dir_all(CONFIG_DIRECTORY) {
        log::warn!("Failed to create config directory: {:?}", error);
    }
}
