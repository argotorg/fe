/// Indentation information for the current rewrite position.
#[derive(Default, Clone, Copy, Debug, Eq, PartialEq)]
pub struct Indent {
    /// Number of spaces representing the current block indentation.
    pub block_indent: usize,
    /// Extra spaces used to align constructs that span multiple lines.
    pub alignment: usize,
}

impl Indent {
    pub fn new(block_indent: usize, alignment: usize) -> Self {
        Self {
            block_indent,
            alignment,
        }
    }

    pub fn from_block(block_indent: usize) -> Self {
        Self {
            block_indent,
            alignment: 0,
        }
    }

    pub fn indent_width(self) -> usize {
        self.block_indent + self.alignment
    }
}

/// Describes the available horizontal space for rewriting a node.
///
/// A [`Shape`] combines the current indentation with the maximum allowed
/// line width. `Rewrite` implementations use it to decide whether they can
/// keep constructs on a single line or need to break them.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct Shape {
    /// Current indentation information.
    pub indent: Indent,
    /// Maximum width of the current line, in characters.
    pub width: usize,
}

impl Shape {
    pub fn with_width(width: usize, indent: Indent) -> Self {
        Self { indent, width }
    }
}
