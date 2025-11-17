use crate::{Rewrite, RewriteContext, Shape};
use parser::ast::{self, prelude::AstNode};

impl Rewrite for ast::Root {
    fn rewrite(&self, context: &RewriteContext<'_>, _shape: Shape) -> Option<String> {
        let range = self.syntax().text_range();
        Some(context.snippet(range).to_owned())
    }
}
