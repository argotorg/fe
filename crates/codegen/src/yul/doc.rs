/// Simple document tree used to build readable Yul output with indentation.
#[derive(Clone)]
pub(super) enum YulDoc {
    /// Single line of text.
    Line(String),
    /// Block with standard indentation (used for most control flow constructs).
    Block { caption: String, body: Vec<YulDoc> },
    /// Block whose contents need wider indentation (useful for `switch case` formatting).
    WideBlock { caption: String, body: Vec<YulDoc> },
}

impl YulDoc {
    /// Convenience helper for creating a `Line`.
    pub(super) fn line(text: impl Into<String>) -> Self {
        YulDoc::Line(text.into())
    }

    /// Convenience helper for creating a `Block`.
    pub(super) fn block(caption: impl Into<String>, body: Vec<YulDoc>) -> Self {
        YulDoc::Block {
            caption: caption.into(),
            body,
        }
    }

    /// Convenience helper for creating a `WideBlock`.
    pub(super) fn wide_block(caption: impl Into<String>, body: Vec<YulDoc>) -> Self {
        YulDoc::WideBlock {
            caption: caption.into(),
            body,
        }
    }
}

/// Recursively renders a YulDoc tree into formatted lines with the requested indentation.
pub(super) fn render_docs(nodes: &[YulDoc], indent: usize, out: &mut Vec<String>) {
    for node in nodes {
        match node {
            YulDoc::Line(text) => {
                if text.is_empty() {
                    out.push(String::new());
                } else {
                    out.push(format!("{}{}", " ".repeat(indent), text));
                }
            }
            YulDoc::Block { caption, body } => {
                let indent_str = " ".repeat(indent);
                out.push(format!("{}{caption}{{", indent_str));
                render_docs(body, indent + 2, out);
                out.push(format!("{}{}", indent_str, "}"));
            }
            YulDoc::WideBlock { caption, body } => {
                let indent_str = " ".repeat(indent);
                out.push(format!("{}{caption}{{", indent_str));
                render_docs(body, indent + 4, out);
                // maintain the leading spaces that were in caption
                let close_pad: String = caption.chars().take_while(|c| c.is_whitespace()).collect();
                out.push(format!("{}{}{}", indent_str, close_pad, "}"));
            }
        }
    }
}
