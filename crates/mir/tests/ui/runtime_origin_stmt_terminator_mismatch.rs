use fe_mir::origin::{RuntimeStmtOrigin, RuntimeTerminatorOrigin};

fn takes_stmt(_: RuntimeStmtOrigin<'_>) {}

fn main() {
    fn mismatch(terminator: RuntimeTerminatorOrigin<'_>) {
        takes_stmt(terminator);
    }
}
