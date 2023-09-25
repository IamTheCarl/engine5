use std::{
    fs::File,
    io::{BufRead, BufReader},
    path::PathBuf,
};

use anyhow::{bail, Context, Result};
use bevy::prelude::*;
use bevy_console::{ConsoleCommandEntered, ConsoleConfiguration};
use shlex::Shlex;

#[derive(Debug, Hash, PartialEq, Eq, Clone, SystemSet)]
pub struct ConsoleScripts;

impl Plugin for ConsoleScripts {
    fn build(&self, app: &mut App) {
        app.add_event::<RequestScript>();
        app.add_systems(Update, run_scripts);
        app.add_systems(Startup, request_startup_script);
    }
}

fn request_startup_script(mut script_requests: EventWriter<RequestScript>) {
    let path = match std::env::var("ENGINE5_STARTUP_SCRIPT") {
        Ok(startup_script) => Some(PathBuf::from(&startup_script)),
        Err(error) => match error {
            std::env::VarError::NotPresent => None,
            std::env::VarError::NotUnicode(path) => Some(PathBuf::from(&path)),
        },
    };

    if let Some(path) = path {
        script_requests.send(RequestScript { path });
    } else {
        log::info!("No startup script specified.");
    }
}

#[derive(Event)]
pub struct RequestScript {
    pub path: PathBuf,
}

fn run_scripts(
    mut script_requests: EventReader<RequestScript>,
    console_config: Res<ConsoleConfiguration>,
    mut command_entered: EventWriter<ConsoleCommandEntered>,
) {
    for request in script_requests.iter() {
        if let Err(error) = run_script(request, &console_config, &mut command_entered) {
            log::error!("Failed to run script {:?}: {:?}", request.path, error);
        }
    }
}

fn run_script(
    request: &RequestScript,
    console_config: &ConsoleConfiguration,
    command_entered: &mut EventWriter<ConsoleCommandEntered>,
) -> Result<()> {
    let script = File::open(&request.path).context("Failed to open script file.")?;
    let script = BufReader::new(script);
    let script = script.lines();

    for (line_index, line) in script.enumerate() {
        let line = line.context("Failed to read next line from file.")?;

        let mut args = Shlex::new(&line).collect::<Vec<_>>();
        if !args.is_empty() {
            let command_name = args.remove(0);
            let command = console_config.commands.get(command_name.as_str());

            if command.is_some() {
                command_entered.send(ConsoleCommandEntered { command_name, args });
            } else {
                bail!("Unknown command on line {}.", line_index)
            }
        }
    }

    Ok(())
}
