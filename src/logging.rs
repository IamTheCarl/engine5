use std::{
    fmt::{self, Write},
    sync::mpsc::{self, TrySendError},
};

use bevy::{
    prelude::*,
    utils::{
        synccell::SyncCell,
        tracing::{field::Field, Subscriber},
    },
};
use bevy_console::{ConsoleSet, PrintConsoleLine};
use tracing_log::LogTracer;
use tracing_subscriber::{field::Visit, layer::Layer, prelude::*, EnvFilter, Registry};

const DEFAULT_LEVEL: log::Level = log::Level::Info;
const DEFAULT_FILTER: &str = "wgpu=error,naga=warn";

struct ConsoleSubscriber {
    sender: mpsc::SyncSender<PrintConsoleLine>,
}

impl ConsoleSubscriber {
    fn write_to_console(
        mut console_line: EventWriter<PrintConsoleLine>,
        mut receiver: ResMut<ConsoleReceiver>,
    ) {
        while let Ok(line) = receiver.receiver.get().try_recv() {
            console_line.send(line);
        }
    }
}

impl<S: Subscriber> Layer<S> for ConsoleSubscriber {
    fn on_event(
        &self,
        event: &bevy::utils::tracing::Event<'_>,
        _ctx: tracing_subscriber::layer::Context<'_, S>,
    ) {
        // I know that some day this project with either be leaked or open source, so I will leave
        // a funny comment for you about my frusteration over writing this function.

        // You see, I had to read documentation for about three hours just to figure out how to pipe and
        // format logs to another file using this stupid library. I am so mad. It's not even a complete implementation.

        pub struct Visitor<'a> {
            string: &'a mut Option<String>,
            tag: &'static str,
        }

        impl<'a> Visit for Visitor<'a> {
            fn record_debug(&mut self, field: &Field, value: &dyn fmt::Debug) {
                if field.name() == self.tag {
                    let mut string = String::new();
                    write!(&mut string, "{:?}", value).unwrap();

                    *self.string = Some(string);
                }
            }
        }

        let metadata = event.metadata();
        let level = metadata.level();

        let mut message = None;
        event.record(&mut Visitor {
            string: &mut message,
            tag: "message",
        });

        let mut module = None;
        event.record(&mut Visitor {
            string: &mut module,
            tag: "log.module_path",
        });

        let mut module = module.unwrap_or_else(|| metadata.target().to_string());
        module.retain(|c| c != '"'); // For some reason the log crate quotes this thing, so take those off.

        if let Some(message) = message {
            let message = format!("{} {}: {:?}", level, module, message);
            if let Err(TrySendError::Full(_attempted_message)) =
                self.sender.try_send(PrintConsoleLine::new(message.into()))
            {
                eprintln!("Too many log messages in one frame. Not all of them can be forwarded to the console.");
            }
        }
    }
}

#[derive(Resource)]
struct ConsoleReceiver {
    receiver: SyncCell<mpsc::Receiver<PrintConsoleLine>>,
}

pub fn setup(app: &mut App) {
    // Setup our logging system.

    let (sender, receiver) = mpsc::sync_channel(500);
    app.insert_resource(ConsoleReceiver {
        receiver: SyncCell::new(receiver),
    });

    let default_filter = { format!("{},{}", DEFAULT_LEVEL, DEFAULT_FILTER) };
    let filter_layer = EnvFilter::try_from_default_env()
        .or_else(|_| EnvFilter::try_new(&default_filter))
        .unwrap();
    let fmt_layer = tracing_subscriber::fmt::Layer::default().with_writer(std::io::stderr);
    let console_subscriber = ConsoleSubscriber { sender };

    let subscriber = Registry::default()
        .with(filter_layer)
        .with(fmt_layer)
        .with(console_subscriber);

    LogTracer::init().expect("Logger was already set.");
    bevy::utils::tracing::subscriber::set_global_default(subscriber)
        .expect("Subscriber was already set.");

    app.add_systems(
        Update,
        ConsoleSubscriber::write_to_console.after(ConsoleSet::ConsoleUI),
    );
}
