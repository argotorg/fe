use crate::{RewriteContext, Shape};

/// Core trait implemented by AST nodes that know how to rewrite themselves
/// into formatted source code.
pub trait Rewrite {
    fn rewrite(&self, context: &RewriteContext<'_>, shape: Shape) -> Option<String>;
}
