//! Runs a batch of inputs under a per-input deadline, so that an input which
//! makes the code under test loop fails the test instead of stalling the suite.

use std::{
    any::Any,
    panic::{AssertUnwindSafe, catch_unwind},
    sync::mpsc::{self, RecvTimeoutError},
    thread,
    time::Duration,
};

/// Feeds each input, in order, to the function that `make` builds.
///
/// The function runs on a worker thread. A panic is recorded and the function
/// is built again, since its state may be corrupted. All panics are reported
/// together at the end, each with its input.
///
/// An input that gives no result within `deadline` of the worker starting it
/// fails the test at once and names that input. Building the function is not
/// part of that time. A thread cannot be stopped, so the stuck worker keeps
/// running until the test process exits.
pub fn run_inputs<F>(
    inputs: impl IntoIterator<Item = String>,
    deadline: Duration,
    make: impl Fn() -> F + Send + 'static,
) where
    F: FnMut(&str),
{
    enum Event {
        Started,
        Finished(Result<(), String>),
    }

    let (input_tx, input_rx) = mpsc::channel::<String>();
    let (event_tx, event_rx) = mpsc::channel::<Event>();

    thread::spawn(move || {
        let mut run = make();
        for input in input_rx {
            if event_tx.send(Event::Started).is_err() {
                return;
            }
            let result = catch_unwind(AssertUnwindSafe(|| run(&input))).map_err(panic_message);
            let panicked = result.is_err();
            if event_tx.send(Event::Finished(result)).is_err() {
                return;
            }
            if panicked {
                run = make();
            }
        }
    });

    let mut panics = Vec::new();
    for input in inputs {
        input_tx.send(input.clone()).expect("worker thread exited");
        let Ok(Event::Started) = event_rx.recv() else {
            panic!("worker thread exited");
        };
        match event_rx.recv_timeout(deadline) {
            Ok(Event::Finished(Ok(()))) => {}
            Ok(Event::Finished(Err(message))) => {
                panics.push(format!("panic on {input:?}: {message}"));
            }
            Ok(Event::Started) => unreachable!("one input is in flight"),
            Err(RecvTimeoutError::Timeout) => {
                panic!("no result after {deadline:?} for input {input:?}, which probably loops")
            }
            Err(RecvTimeoutError::Disconnected) => panic!("worker thread exited"),
        }
    }

    assert!(
        panics.is_empty(),
        "{} input(s) panicked:\n{}",
        panics.len(),
        panics.join("\n")
    );
}

fn panic_message(payload: Box<dyn Any + Send>) -> String {
    payload
        .downcast_ref::<&str>()
        .copied()
        .or_else(|| payload.downcast_ref::<String>().map(String::as_str))
        .unwrap_or("<non-string panic>")
        .to_owned()
}

#[cfg(test)]
mod tests {
    use std::{
        panic::catch_unwind,
        sync::{
            Arc,
            atomic::{AtomicUsize, Ordering},
        },
        thread::sleep,
    };

    use super::*;

    fn inputs<const N: usize>(inputs: [&str; N]) -> Vec<String> {
        inputs.map(String::from).into()
    }

    #[test]
    fn building_the_function_is_not_part_of_the_deadline() {
        run_inputs(inputs(["a"]), Duration::from_secs(1), || {
            sleep(Duration::from_millis(1500));
            |_: &str| {}
        });
    }

    #[test]
    #[should_panic(expected = "no result after")]
    fn an_input_that_runs_past_the_deadline_fails() {
        run_inputs(inputs(["a"]), Duration::from_millis(100), || {
            |_: &str| sleep(Duration::from_secs(5))
        });
    }

    #[test]
    fn a_panic_is_reported_with_its_input_and_the_function_is_built_again() {
        let builds = Arc::new(AtomicUsize::new(0));
        let counter = builds.clone();
        let failure = catch_unwind(|| {
            run_inputs(
                inputs(["a", "boom", "c"]),
                Duration::from_secs(5),
                move || {
                    counter.fetch_add(1, Ordering::SeqCst);
                    |input: &str| assert_ne!(input, "boom")
                },
            );
        })
        .expect_err("the panic on `boom` should fail the run");

        let message = panic_message(failure);
        assert!(message.contains("panic on \"boom\""), "{message}");
        assert_eq!(builds.load(Ordering::SeqCst), 2);
    }
}
