use fe_mir::origin::{RuntimeStmtOrigin, RuntimeTerminatorOrigin};

fn main() {
    fn mismatch(stmt: RuntimeStmtOrigin<'_>, terminator: RuntimeTerminatorOrigin<'_>) {
        let _ = stmt.export_key("runtime:test");
        let _ = terminator.export_key("runtime:test");
    }
}
