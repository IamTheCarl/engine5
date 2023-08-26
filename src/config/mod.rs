use std::path::Path;

use anyhow::{Context, Result};
use bevy::prelude::*;
use serde::{de::DeserializeOwned, Serialize};

pub mod controls;
pub mod file_paths;
pub mod graphics;

#[derive(Debug, Hash, PartialEq, Eq, Clone, SystemSet)]
pub struct LoadConfigSet;

#[derive(Debug, Hash, PartialEq, Eq, Clone, SystemSet)]
struct LoadConfigPreSet;

pub trait Config: Sized + Default + Serialize + DeserializeOwned + Resource {
    const CONFIG_FILE: &'static str;

    fn load() -> Result<Self> {
        let input_map = std::fs::read_to_string(
            Path::new(file_paths::CONFIG_DIRECTORY).join(Self::CONFIG_FILE),
        )
        .context("Failed to open config file.")?;

        let input_map: Self =
            serde_yaml::from_str(&input_map).context("Failed to deserialize config file.")?;

        Ok(input_map)
    }

    fn save(&self) -> Result<()> {
        let input_map = serde_yaml::to_string(self).context("Failed to serialize config file.")?;
        std::fs::write(
            Path::new(file_paths::CONFIG_DIRECTORY).join(Self::CONFIG_FILE),
            input_map,
        )
        .context("Failed to write to config file")?;

        Ok(())
    }

    fn load_or_default() -> Self {
        match Self::load() {
            Ok(config) => {
                log::info!("Loaded config file  `{}`.", Self::CONFIG_FILE);
                config
            }
            Err(error) => {
                log::error!(
                    "Failed to load config file `{}`: {:?}",
                    Self::CONFIG_FILE,
                    error
                );
                log::info!("A new config file will be overwritten to `{}`, replacing the old one if present.", Self::CONFIG_FILE);
                let config = Self::default();

                if let Err(error) = config.save() {
                    log::error!("Failed to save `{}`: {:?}", Self::CONFIG_FILE, error);
                }

                config
            }
        }
    }

    fn app_setup(app: &mut App) {
        let load_config = |mut commands: Commands| {
            commands.insert_resource(Self::load_or_default());
        };

        let save_on_modify = |config: Res<Self>| {
            if config.is_changed() {
                if let Err(error) = config.save() {
                    log::error!(
                        "Failed to save config file `{}`: {:?}",
                        Self::CONFIG_FILE,
                        error
                    );
                }
            }
        };

        app.add_systems(Startup, load_config.in_set(LoadConfigPreSet));
        app.add_systems(Update, save_on_modify.after(load_config));
    }
}

pub struct ConfigPlugin;

impl Plugin for ConfigPlugin {
    fn build(&self, app: &mut App) {
        app.configure_set(Startup, LoadConfigPreSet);
        app.configure_set(Startup, LoadConfigSet.after(LoadConfigPreSet));

        app.add_systems(
            Startup,
            apply_deferred.after(LoadConfigPreSet).before(LoadConfigSet),
        );

        app.add_plugins((
            controls::PlayerControlsPlugin,
            graphics::GraphicsConfigPlugin,
        ));
    }
}
