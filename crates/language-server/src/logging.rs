use async_lsp::{
    ClientSocket, LanguageClient,
    lsp_types::{LogMessageParams, MessageType},
};
use tracing::{Level, Metadata, subscriber::set_default};
use tracing_subscriber::{EnvFilter, fmt::MakeWriter, layer::SubscriberExt};

use std::{backtrace::Backtrace, sync::Arc};

pub fn setup_default_subscriber(client: ClientSocket) -> Option<tracing::subscriber::DefaultGuard> {
    let client_socket_writer = ClientSocketWriterMaker::new(client);

    // Filter out verbose Salsa query logs while keeping our INFO logs
    let filter = EnvFilter::new("info").add_directive("salsa=warn".parse().unwrap());

    // Use fmt layer which properly calls make_writer_for() for correct LSP log levels
    let fmt_layer = tracing_subscriber::fmt::layer()
        .with_ansi(false)
        .with_target(true)
        .with_writer(client_socket_writer);

    let subscriber = tracing_subscriber::registry().with(filter).with(fmt_layer);
    Some(set_default(subscriber))
}

pub fn init_fn(client: ClientSocket) -> impl FnOnce() -> Option<tracing::subscriber::DefaultGuard> {
    move || setup_default_subscriber(client)
}

pub(crate) fn setup_panic_hook() {
    // Set up a panic hook
    std::panic::set_hook(Box::new(|panic_info| {
        // Extract the panic message
        let payload = panic_info.payload();
        let message = if let Some(s) = payload.downcast_ref::<&str>() {
            *s
        } else if let Some(s) = payload.downcast_ref::<String>() {
            &s[..]
        } else {
            "Unknown panic message"
        };

        // Get the location of the panic if available
        let location = if let Some(location) = panic_info.location() {
            format!(" at {}:{}", location.file(), location.line())
        } else {
            String::from("Unknown location")
        };

        // Capture the backtrace
        let backtrace = Backtrace::capture();

        // Log the panic information and backtrace
        tracing::error!(
            "Panic occurred{}: {}\nBacktrace:\n{:?}",
            location,
            message,
            backtrace
        );
    }));
}

pub(crate) struct ClientSocketWriterMaker {
    pub(crate) client_socket: Arc<ClientSocket>,
}

impl ClientSocketWriterMaker {
    pub fn new(client_socket: ClientSocket) -> Self {
        ClientSocketWriterMaker {
            client_socket: Arc::new(client_socket),
        }
    }
}

pub(crate) struct ClientSocketWriter {
    client_socket: Arc<ClientSocket>,
    typ: MessageType,
}

impl std::io::Write for ClientSocketWriter {
    fn write(&mut self, buf: &[u8]) -> std::io::Result<usize> {
        let message = String::from_utf8_lossy(buf).to_string();
        let params = LogMessageParams {
            typ: self.typ,
            message,
        };

        let mut client_socket = self.client_socket.as_ref();
        _ = client_socket.log_message(params);
        Ok(buf.len())
    }

    fn flush(&mut self) -> std::io::Result<()> {
        Ok(())
    }
}

impl<'a> MakeWriter<'a> for ClientSocketWriterMaker {
    type Writer = ClientSocketWriter;

    fn make_writer(&'a self) -> Self::Writer {
        ClientSocketWriter {
            client_socket: self.client_socket.clone(),
            typ: MessageType::LOG,
        }
    }

    fn make_writer_for(&'a self, meta: &Metadata<'_>) -> Self::Writer {
        let typ = match *meta.level() {
            Level::ERROR => MessageType::ERROR,
            Level::WARN => MessageType::WARNING,
            Level::INFO => MessageType::INFO,
            Level::DEBUG => MessageType::LOG,
            Level::TRACE => MessageType::LOG,
        };

        ClientSocketWriter {
            client_socket: self.client_socket.clone(),
            typ,
        }
    }
}
