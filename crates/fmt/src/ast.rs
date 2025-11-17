use crate::{Rewrite, RewriteContext, Shape};
use parser::{
    TextRange,
    ast::{self, ItemKind, prelude::AstNode},
    syntax_node::NodeOrToken,
};

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
        match self.kind()? {
            ItemKind::Func(func) => func.rewrite(context, _shape),
            _ => {
                let range = self.syntax().text_range();
                Some(context.snippet(range).to_owned())
            }
        }
    }
}

impl Rewrite for ast::Func {
    fn rewrite(&self, context: &RewriteContext<'_>, shape: Shape) -> Option<String> {
        let func_range = self.syntax().text_range();
        let body = self.body()?;
        let body_range = body.syntax().text_range();

        // Split the function into:
        //   [prefix: signature and attributes][body block][suffix: trailing trivia]
        let prefix_range = TextRange::new(func_range.start(), body_range.start());
        let suffix_range = TextRange::new(body_range.end(), func_range.end());

        let prefix = context.snippet(prefix_range);
        let suffix = context.snippet(suffix_range);

        let formatted_body = body.rewrite(context, shape)?;

        let mut out = String::new();
        out.push_str(prefix);
        out.push_str(&formatted_body);
        out.push_str(suffix);
        Some(out)
    }
}

impl Rewrite for ast::BlockExpr {
    fn rewrite(&self, context: &RewriteContext<'_>, shape: Shape) -> Option<String> {
        let range = self.syntax().text_range();
        let snippet = context.snippet(range);

        // Expect a `{ .. }` block. If we can't find matching braces, fall
        // back to the original source.
        let open_idx = snippet.find('{')?;
        let close_idx = snippet.rfind('}')?;
        if open_idx >= close_idx {
            return Some(snippet.to_owned());
        }

        let interior = &snippet[open_idx + 1..close_idx];
        let outer_indent = shape.indent.indent_width();
        let indent_width = context.config.indent_width;

        let mut out = String::new();
        out.push('{');

        // Re-indent the interior of the block.
        let mut level: i32 = 1;
        let mut had_any_line = false;

        for raw_line in interior.lines() {
            let content = raw_line.trim();

            if content.is_empty() {
                out.push('\n');
                continue;
            }

            had_any_line = true;

            // If the line starts with one or more closing braces, outdent it.
            let mut close_before_code = 0;
            for ch in content.chars() {
                if ch == '}' {
                    close_before_code += 1;
                } else if ch.is_whitespace() {
                    continue;
                } else {
                    break;
                }
            }

            let line_level = std::cmp::max(0, level - close_before_code) as usize;
            let indent = outer_indent + line_level * indent_width;
            for _ in 0..indent {
                out.push(' ');
            }

            out.push_str(content);
            out.push('\n');

            // Update indentation level for subsequent lines.
            for ch in content.chars() {
                match ch {
                    '{' => level += 1,
                    '}' => {
                        level -= 1;
                        if level < 0 {
                            level = 0;
                        }
                    }
                    _ => {}
                }
            }
        }

        if !had_any_line {
            out.push('}');
        } else {
            // Close the block on its own line with the outer indentation.
            for _ in 0..outer_indent {
                out.push(' ');
            }
            out.push('}');
        }

        Some(out)
    }
}
