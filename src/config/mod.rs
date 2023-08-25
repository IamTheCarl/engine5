use std::path::Path;

use anyhow::{Context, Result};
use serde::{de::DeserializeOwned, Serialize};

pub mod controls;
pub mod file_paths;
pub mod graphics;

trait Config: Sized + Default + Serialize + DeserializeOwned {
    const CONFIG_FILE: &'static str;

    fn load() -> Result<Self> {
        let input_map = std::fs::read_to_string(
            Path::new(file_paths::CONFIG_DIRECTORY).join(Self::CONFIG_FILE),
        )
        .with_context(|| format!("Failed to open config file `{}`", Self::CONFIG_FILE))?;

        let input_map: Self = serde_yaml::from_str(&input_map).with_context(|| {
            format!("Failed to deserialize config file `{}`", Self::CONFIG_FILE)
        })?;

        Ok(input_map)
    }

    fn save(&self) -> Result<()> {
        let input_map = serde_yaml::to_string(self)
            .with_context(|| format!("Failed to serialize config file `{}`", Self::CONFIG_FILE))?;
        std::fs::write(
            Path::new(file_paths::CONFIG_DIRECTORY).join(Self::CONFIG_FILE),
            input_map,
        )
        .with_context(|| format!("Failed to write to config file `{}`", Self::CONFIG_FILE))?;

        Ok(())
    }

    fn load_or_default() -> Self {
        match Self::load() {
            Ok(map) => map,
            Err(error) => {
                log::error!(
                    "Failed to load config file `{}`: {:?}",
                    Self::CONFIG_FILE,
                    error
                );
                log::info!("A new config file will be overwritten to `{}`, replacing the old one if present.", Self::CONFIG_FILE);
                let map = Self::default();

                if let Err(error) = map.save() {
                    log::error!("Failed to save `{}`: {:?}", Self::CONFIG_FILE, error);
                }

                map
            }
        }
    }
}
