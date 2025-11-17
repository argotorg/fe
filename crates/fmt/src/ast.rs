use crate::{Rewrite, RewriteContext, Shape};
use parser::ast::{self, prelude::AstNode};
use parser::syntax_node::NodeOrToken;

impl Rewrite for ast::Root {
    fn rewrite(&self, context: &RewriteContext<'_>, shape: Shape) -> Option<String> {
        let mut out = String::new();
        for child in self.syntax().children_with_tokens() {
            match child {
                NodeOrToken::Node(node) => {
                    if let Some(item_list) = ast::ItemList::cast(node.clone()) {
                        if let Some(formatted) = item_list.rewrite(context, shape) {
                            out.push_str(&formatted);
                        } else {
                            out.push_str(context.snippet(node.text_range()));
                        }
                    } else {
                        out.push_str(context.snippet(node.text_range()));
                    }
                }
                NodeOrToken::Token(token) => {
                    out.push_str(context.snippet(token.text_range()));
                }
            }
        }
        Some(out)
    }
}

impl Rewrite for ast::ItemList {
    fn rewrite(&self, context: &RewriteContext<'_>, shape: Shape) -> Option<String> {
        let mut out = String::new();
        for child in self.syntax().children_with_tokens() {
            match child {
                NodeOrToken::Node(node) => {
                    if let Some(item) = ast::Item::cast(node.clone()) {
                        if let Some(formatted) = item.rewrite(context, shape) {
                            out.push_str(&formatted);
                        } else {
                            out.push_str(context.snippet(node.text_range()));
                        }
                    } else {
                        out.push_str(context.snippet(node.text_range()));
                    }
                }
                NodeOrToken::Token(token) => {
                    out.push_str(context.snippet(token.text_range()));
                }
            }
        }
        Some(out)
    }
}

impl Rewrite for ast::Item {
    fn rewrite(&self, context: &RewriteContext<'_>, _shape: Shape) -> Option<String> {
        let range = self.syntax().text_range();
        Some(context.snippet(range).to_owned())
    }
}
