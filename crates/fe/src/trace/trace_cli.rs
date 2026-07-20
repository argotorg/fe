use crate::{DevCommand, DevTraceCommand};

pub(crate) fn run_dev_command(command: &DevCommand) -> Result<String, String> {
    match command {
        DevCommand::Trace { command } => run_dev_trace_command(command),
    }
}

fn run_dev_trace_command(command: &DevTraceCommand) -> Result<String, String> {
    match command {
        DevTraceCommand::Emit(args) => super::trace_emit::run_trace_emit(args),
        DevTraceCommand::Validate(args) => super::trace_emit::run_trace_validate(args),
    }
}
