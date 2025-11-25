use crate::{Indent, RewriteContext, Shape, rewrite::RewriteExt};

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum ListTactic {
    /// Always format horizontally.
    Horizontal,
    /// Always format vertically.
    Vertical,
    /// Try horizontal, fallback to vertical if it exceeds width.
    Mixed,
}

#[derive(Clone, Debug)]
pub struct ListFormatting<'a> {
    pub tactic: ListTactic,
    pub separator: &'a str,
    pub trailing_separator: bool,
    pub surround: Option<(&'a str, &'a str)>,
}

impl<'a> Default for ListFormatting<'a> {
    fn default() -> Self {
        Self {
            tactic: ListTactic::Mixed,
            separator: ",",
            trailing_separator: true,
            surround: None,
        }
    }
}

impl<'a> ListFormatting<'a> {
    pub fn new(_shape: Shape) -> Self {
        Self::default()
    }

    pub fn separator(mut self, separator: &'a str) -> Self {
        self.separator = separator;
        self
    }

    pub fn trailing_separator(mut self, trailing: bool) -> Self {
        self.trailing_separator = trailing;
        self
    }

    pub fn with_surround(mut self, left: &'a str, right: &'a str) -> Self {
        self.surround = Some((left, right));
        self
    }
    
    pub fn tactic(mut self, tactic: ListTactic) -> Self {
        self.tactic = tactic;
        self
    }
}

pub fn format_list<T>(
    items: impl IntoIterator<Item = T>,
    formatting: &ListFormatting<'_>,
    context: &RewriteContext<'_>,
    shape: Shape,
) -> Option<String>
where
    T: RewriteExt,
{
    let items: Vec<T> = items.into_iter().collect();
    let (open, close) = formatting.surround.unwrap_or(("", ""));
    let available_width = shape
        .width
        .saturating_sub(shape.indent.indent_width() + open.len() + close.len());
    
    if items.is_empty() {
        return Some(format!("{}{}", open, close));
    }

    // 1. Try Horizontal
    if matches!(formatting.tactic, ListTactic::Horizontal | ListTactic::Mixed) {
        let item_strings: Vec<String> = items
            .iter()
            .map(|item| {
                // For horizontal, we assume items fit on the same line, so we pass the current shape
                // (or maybe we should reduce width? but we verify total length later).
                item.rewrite_or_original(context, shape)
            })
            .collect();

        let joined = item_strings.join(&format!("{} ", formatting.separator));
        let inline_len = joined.len();

        if inline_len <= available_width {
            let trailing = if formatting.trailing_separator
                && matches!(formatting.tactic, ListTactic::Horizontal)
            {
                formatting.separator
            } else {
                ""
            };
            let result = format!("{open}{joined}{trailing}{close}");

            if formatting.tactic == ListTactic::Horizontal {
                return Some(result);
            }

            return Some(result);
        }
    }
    
    // 2. Vertical
    let indent_width = context.config.indent_width;
    let item_indent = shape.indent.block_indent + indent_width;
    let item_shape = Shape::with_width(shape.width, Indent::from_block(item_indent));
    
    let mut out = String::new();
    out.push_str(open);
    out.push('\n');
    
    for (idx, item) in items.iter().enumerate() {
        push_indent(&mut out, item_indent);
        out.push_str(&item.rewrite_or_original(context, item_shape));
        if formatting.trailing_separator || idx + 1 != items.len() {
            out.push_str(formatting.separator);
        }
        out.push('\n');
    }
    
    push_indent(&mut out, shape.indent.block_indent);
    out.push_str(close);
    
    Some(out)
}

fn push_indent(out: &mut String, width: usize) {
    for _ in 0..width {
        out.push(' ');
    }
}
