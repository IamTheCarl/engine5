use bevy::prelude::*;
use std::fmt::Write;
use tracing::{field::Field, Event, Subscriber};
use tracing_log::LogTracer;
use tracing_subscriber::{field::Visit, layer::Layer, prelude::*, EnvFilter, Registry};

use bevy_console::PrintConsoleLine;

const DEFAULT_LEVEL: log::Level = log::Level::Info;
const DEFAULT_FILTER: &str = "wgpu=error,naga=warn";

struct ConsoleSubscriber {
    sender: std::sync::mpsc::SyncSender<PrintConsoleLine>,
}

impl<S: Subscriber> Layer<S> for ConsoleSubscriber {
    fn on_event(&self, event: &Event<'_>, _ctx: tracing_subscriber::layer::Context<'_, S>) {
        pub struct Visitor<'a> {
            string: &'a mut Option<String>,
            tag: &'static str,
        }

        impl<'a> Visit for Visitor<'a> {
            fn record_debug(&mut self, field: &Field, value: &dyn std::fmt::Debug) {
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
        module.retain(|c| c != '"');

        if let Some(message) = message {
            let message = format!("{} {}: {:?}", level, module, message);
            if let Err(std::sync::mpsc::TrySendError::Full(_attempted_message)) =
                self.sender.try_send(PrintConsoleLine::new(message.into()))
            {
                eprintln!("Too many log messages in one frame. Not all of them can be forwarded to the console.");
            }
        }
    }
}

pub fn setup(app: &mut App) {
    let (sender, receiver) = std::sync::mpsc::sync_channel(500);

    app.insert_resource(ConsoleReceiver {
        receiver: bevy::platform::cell::SyncCell::new(receiver),
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
    tracing::subscriber::set_global_default(subscriber).expect("Subscriber was already set.");

    app.add_systems(
        Update,
        ConsoleSubscriber::write_to_console.in_set(bevy_console::ConsoleSet::PostCommands),
    );
}

#[derive(Resource)]
struct ConsoleReceiver {
    receiver: bevy::platform::cell::SyncCell<std::sync::mpsc::Receiver<PrintConsoleLine>>,
}

impl ConsoleSubscriber {
    fn write_to_console(
        mut receiver: ResMut<ConsoleReceiver>,
        mut console_line: MessageWriter<PrintConsoleLine>,
    ) {
        let mut buffer = Vec::new();
        while let Ok(line) = receiver.receiver.get().try_recv() {
            buffer.push(line);
        }
        for line in buffer {
            console_line.write(PrintConsoleLine::new(line.line));
        }
    }
}
