use parser::ast::prelude::AstNode;

use crate::{RewriteContext, Shape};

/// Core trait implemented by AST nodes that know how to rewrite themselves
/// into formatted source code.
pub trait Rewrite {
    fn rewrite(&self, context: &RewriteContext<'_>, shape: Shape) -> Option<String>;
}

pub trait RewriteExt: Rewrite + AstNode {
    fn rewrite_or_original(&self, context: &RewriteContext<'_>, shape: Shape) -> String {
        self.rewrite(context, shape).unwrap_or_else(|| {
            context
                .snippet(self.syntax().text_range())
                .trim()
                .to_owned()
        })
    }
}

impl<T: Rewrite + AstNode> RewriteExt for T {}
