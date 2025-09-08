#[doc(hidden)]
pub mod _macro_support;
pub mod snap;
pub mod url_utils;
pub use tracing::Level;
use tracing::{
    level_filters::LevelFilter,
    subscriber::{set_default, DefaultGuard},
    Subscriber,
};
use tracing_subscriber::{layer::SubscriberExt, EnvFilter};
use tracing_tree::HierarchicalLayer;

pub fn setup_tracing_with_filter(filter: &str) -> DefaultGuard {
    let subscriber = default_subscriber().with(EnvFilter::new(filter));
    set_default(subscriber)
}

pub fn setup_tracing(level: Level) -> DefaultGuard {
    let subscriber = default_subscriber().with(LevelFilter::from_level(level));
    set_default(subscriber)
}

fn default_subscriber() -> impl Subscriber + Send + Sync {
    tracing_subscriber::registry().with(
        HierarchicalLayer::new(2)
            .with_targets(true)
            .with_thread_ids(true)
            .with_thread_names(true)
            .with_indent_lines(true)
            .with_bracketed_fields(true)
            .with_ansi(false)
            .with_writer(std::io::stderr),
    )
}

#[cfg(test)]
mod tests {
    use super::*;
    use tracing::{debug, info, warn};

    #[test]
    fn test_rust_log_env() {
        // This test demonstrates how to use RUST_LOG with tests
        // Run with: RUST_LOG=debug cargo test test_rust_log_env -- --nocapture
        let _guard = setup_tracing_with_filter(
            &std::env::var("RUST_LOG").unwrap_or_else(|_| "off".to_string()),
        );

        info!("This is an info message");
        debug!("This is a debug message");
        warn!("This is a warning message");

        // Test passes if no panic occurs
    }
}
