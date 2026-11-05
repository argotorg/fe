#[doc(hidden)]
pub mod _macro_support;
pub mod deadline;
pub mod fixtures;
pub mod normalize;
pub mod url_utils;
pub use tracing::Level;
use tracing::{
    Subscriber,
    level_filters::LevelFilter,
    subscriber::{DefaultGuard, set_default},
};
use tracing_subscriber::{EnvFilter, fmt::TestWriter, layer::SubscriberExt};
use tracing_tree::HierarchicalLayer;

/// Lowers this process's core size limit to 1, which the programs it runs
/// inherit. A limit of 1 writes no core file and, unlike 0, keeps the kernel
/// from piping each crash to a crash reporter such as apport, which takes
/// about a second per crash. Tests that run programs which trap call it first.
#[cfg(unix)]
pub fn skip_crash_reports() {
    static LOWERED: std::sync::Once = std::sync::Once::new();
    LOWERED.call_once(|| {
        let mut limit = libc::rlimit {
            rlim_cur: 0,
            rlim_max: 0,
        };
        unsafe {
            assert_eq!(libc::getrlimit(libc::RLIMIT_CORE, &mut limit), 0);
            limit.rlim_max = limit.rlim_max.min(1);
            limit.rlim_cur = limit.rlim_max;
            assert_eq!(libc::setrlimit(libc::RLIMIT_CORE, &limit), 0);
        }
    });
}

pub fn setup_tracing_with_filter(filter: &str) -> DefaultGuard {
    let subscriber = default_subscriber().with(EnvFilter::new(filter));
    set_default(subscriber)
}

pub fn setup_tracing(level: Level) -> DefaultGuard {
    let subscriber = default_subscriber().with(LevelFilter::from_level(level));
    set_default(subscriber)
}

pub fn setup_test_tracing() -> DefaultGuard {
    let subscriber = test_subscriber();
    set_default(subscriber)
}

pub fn test_subscriber() -> impl Subscriber + Send + Sync {
    tracing_subscriber::registry()
        .with(
            EnvFilter::builder()
                .with_default_directive(LevelFilter::INFO.into())
                .from_env_lossy(),
        )
        .with(
            HierarchicalLayer::new(2)
                .with_targets(true)
                .with_thread_ids(true)
                .with_thread_names(true)
                .with_indent_lines(true)
                .with_bracketed_fields(true)
                .with_ansi(false)
                .with_writer(TestWriter::default()),
        )
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
