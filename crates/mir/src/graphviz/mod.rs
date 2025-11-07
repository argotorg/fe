use std::io;

use fe_analyzer::namespace::items::ModuleId;

use crate::db::MirDb;

mod block;
mod function;
mod module;

/// Writes mir graphs of functions in a `module`.
pub fn write_mir_graphs<W: io::Write>(
    db: &dyn MirDb,
    module: ModuleId,
    w: &mut W,
) -> io::Result<()> {
    let module_graph = module::ModuleGraph::new(db, module);
    dot2::render(&module_graph, w).map_err(|err| match err {
        dot2::Error::Io(err) => err,
        _ => panic!("invalid graphviz id"),
    })
}
