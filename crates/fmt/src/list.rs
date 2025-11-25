use crate::{Indent, RewriteContext, Shape, config::TrailingComma, rewrite::RewriteExt};

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
    /// Override trailing separator behavior. `None` means use config default.
    pub trailing_separator: Option<bool>,
    pub surround: Option<(&'a str, &'a str)>,
    /// Add space padding inside surrounds for horizontal mode (e.g., `{ a, b }` instead of `{a, b}`).
    pub horizontal_padding: bool,
}

impl<'a> Default for ListFormatting<'a> {
    fn default() -> Self {
        Self {
            tactic: ListTactic::Mixed,
            separator: ",",
            trailing_separator: None,
            surround: None,
            horizontal_padding: false,
        }
    }
}

impl<'a> ListFormatting<'a> {
    pub fn new(_shape: Shape) -> Self {
        // Note: shape parameter kept for API compatibility but currently unused.
        // Future enhancement: could use shape.width to choose default tactic.
        Self::default()
    }

    pub fn separator(mut self, separator: &'a str) -> Self {
        self.separator = separator;
        self
    }

    pub fn trailing_separator(mut self, trailing: bool) -> Self {
        self.trailing_separator = Some(trailing);
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

    pub fn horizontal_padding(mut self, padding: bool) -> Self {
        self.horizontal_padding = padding;
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

    // Resolve trailing separator behavior from override or config.
    let trailing_for_horizontal = formatting.trailing_separator.unwrap_or_else(|| {
        matches!(context.config.trailing_comma, TrailingComma::Always)
    });
    let trailing_for_vertical = formatting.trailing_separator.unwrap_or_else(|| {
        !matches!(context.config.trailing_comma, TrailingComma::Never)
    });

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
            let trailing = if trailing_for_horizontal {
                formatting.separator
            } else {
                ""
            };
            let (pad_left, pad_right) = if formatting.horizontal_padding {
                (" ", " ")
            } else {
                ("", "")
            };
            let result = format!("{open}{pad_left}{joined}{trailing}{pad_right}{close}");

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
        if trailing_for_vertical || idx + 1 != items.len() {
            out.push_str(formatting.separator);
        }
        out.push('\n');
    }

    push_indent(&mut out, shape.indent.block_indent);
    out.push_str(close);

    Some(out)
}

fn push_indent(out: &mut String, width: usize) {
    Indent::push_to(width, out);
}
