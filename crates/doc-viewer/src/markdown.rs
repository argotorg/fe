//! Markdown to HTML rendering with syntax highlighting

use pulldown_cmark::{html, Event, Options, Parser, Tag, TagEnd};

/// Render markdown to HTML with Fe syntax highlighting for code blocks
pub fn render_markdown(markdown: &str) -> String {
    let mut options = Options::empty();
    options.insert(Options::ENABLE_STRIKETHROUGH);
    options.insert(Options::ENABLE_TABLES);

    // Convert single newlines to hard breaks (two trailing spaces)
    // This preserves line breaks in doc comments as users expect
    let processed = preserve_line_breaks(markdown);

    let parser = Parser::new_ext(&processed, options);

    // Process events, handling code blocks specially
    let parser = CodeBlockHighlighter::new(parser);

    let mut html_output = String::new();
    html::push_html(&mut html_output, parser);

    html_output
}

/// Iterator adapter that highlights code blocks
struct CodeBlockHighlighter<'a, I> {
    inner: I,
    in_code_block: bool,
    code_lang: Option<String>,
    code_buffer: String,
    pending_events: Vec<Event<'a>>,
}

impl<'a, I> CodeBlockHighlighter<'a, I>
where
    I: Iterator<Item = Event<'a>>,
{
    fn new(inner: I) -> Self {
        Self {
            inner,
            in_code_block: false,
            code_lang: None,
            code_buffer: String::new(),
            pending_events: Vec::new(),
        }
    }

    fn highlight_code(&self, code: &str, lang: Option<&str>) -> String {
        // For now, just wrap in <pre><code> with language class
        // TODO: integrate syntect for proper highlighting
        let lang_class = lang
            .map(|l| format!(" class=\"language-{}\"", l))
            .unwrap_or_default();

        format!(
            "<pre><code{}>{}</code></pre>",
            lang_class,
            html_escape(code)
        )
    }
}

impl<'a, I> Iterator for CodeBlockHighlighter<'a, I>
where
    I: Iterator<Item = Event<'a>>,
{
    type Item = Event<'a>;

    fn next(&mut self) -> Option<Self::Item> {
        // Return pending events first
        if let Some(event) = self.pending_events.pop() {
            return Some(event);
        }

        loop {
            let event = self.inner.next()?;

            match &event {
                Event::Start(Tag::CodeBlock(kind)) => {
                    self.in_code_block = true;
                    self.code_buffer.clear();
                    self.code_lang = match kind {
                        pulldown_cmark::CodeBlockKind::Fenced(lang) => {
                            let lang_str = lang.as_ref();
                            if lang_str.is_empty() {
                                None
                            } else {
                                Some(lang_str.to_string())
                            }
                        }
                        pulldown_cmark::CodeBlockKind::Indented => None,
                    };
                    continue;
                }
                Event::End(TagEnd::CodeBlock) => {
                    self.in_code_block = false;
                    let highlighted =
                        self.highlight_code(&self.code_buffer, self.code_lang.as_deref());
                    return Some(Event::Html(highlighted.into()));
                }
                Event::Text(text) if self.in_code_block => {
                    self.code_buffer.push_str(text);
                    continue;
                }
                _ => return Some(event),
            }
        }
    }
}

/// Convert single newlines to markdown hard breaks (two trailing spaces + newline)
/// This preserves line breaks in doc comments while still allowing paragraph breaks (double newlines)
fn preserve_line_breaks(text: &str) -> String {
    let mut result = String::with_capacity(text.len() * 2);
    let mut in_code_block = false;
    let mut prev_was_newline = false;

    for line in text.lines() {
        // Track code block state
        if line.trim().starts_with("```") {
            in_code_block = !in_code_block;
        }

        if !result.is_empty() {
            if prev_was_newline {
                // Double newline - keep as paragraph break
                result.push('\n');
            } else if !in_code_block && !line.is_empty() {
                // Single newline outside code block - add hard break
                result.push_str("  \n");
            } else {
                result.push('\n');
            }
        }

        prev_was_newline = line.is_empty();
        result.push_str(line);
    }

    result
}

/// Escape HTML special characters
fn html_escape(s: &str) -> String {
    s.replace('&', "&amp;")
        .replace('<', "&lt;")
        .replace('>', "&gt;")
        .replace('"', "&quot;")
        .replace('\'', "&#x27;")
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_basic_markdown() {
        let md = "# Hello\n\nThis is a **test**.";
        let html = render_markdown(md);
        assert!(html.contains("<h1>Hello</h1>"));
        assert!(html.contains("<strong>test</strong>"));
    }

    #[test]
    fn test_code_block() {
        let md = "```fe\nfn main() {}\n```";
        let html = render_markdown(md);
        assert!(html.contains("language-fe"));
        assert!(html.contains("fn main()"));
    }

    #[test]
    fn test_multiline_preserves_breaks() {
        let md = "Line 1\nLine 2\nLine 3";
        let html = render_markdown(md);
        // Each line should have a <br> tag (from hard break)
        assert!(html.contains("<br"));
    }

    #[test]
    fn test_paragraph_breaks_preserved() {
        let md = "Paragraph 1\n\nParagraph 2";
        let html = render_markdown(md);
        // Should have two <p> tags
        assert!(html.matches("<p>").count() == 2);
    }
}
