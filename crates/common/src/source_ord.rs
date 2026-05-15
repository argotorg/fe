use salsa::Update;

pub fn byte_offset_to_line_col(text: &str, offset: u32) -> (u32, u32) {
    let offset = offset as usize;
    let mut line = 1u32;
    let mut col = 1u32;
    for (i, ch) in text.char_indices() {
        if i >= offset {
            break;
        }
        if ch == '\n' {
            line += 1;
            col = 1;
        } else {
            col += 1;
        }
    }
    (line, col)
}

#[derive(Clone, Copy, Debug, Default, PartialEq, Eq, Hash, PartialOrd, Ord, Update)]
pub struct SourceOrd(u32);

impl SourceOrd {
    pub fn new(index: u32) -> Self {
        Self(index)
    }

    pub fn is_default(self) -> bool {
        self == Self::default()
    }

    pub fn index(self) -> u32 {
        self.0
    }
}

#[derive(Clone, Debug, Default, PartialEq, Eq, Hash, Update)]
pub struct FunctionSourceTable {
    entries: Vec<SourceEntry>,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash, Update)]
pub struct SourceEntry {
    pub file_path: String,
    pub start_line: u32,
    pub start_col: u32,
    pub end_line: u32,
    pub end_col: u32,
}

impl FunctionSourceTable {
    pub fn new() -> Self {
        Self {
            entries: Vec::new(),
        }
    }

    pub fn push(
        &mut self,
        file_path: String,
        start_line: u32,
        start_col: u32,
        end_line: u32,
        end_col: u32,
    ) -> SourceOrd {
        let ord = SourceOrd::new(self.entries.len() as u32);
        self.entries.push(SourceEntry {
            file_path,
            start_line,
            start_col,
            end_line,
            end_col,
        });
        ord
    }

    pub fn get(&self, ord: SourceOrd) -> Option<&SourceEntry> {
        self.entries.get(ord.index() as usize)
    }

    pub fn len(&self) -> usize {
        self.entries.len()
    }

    pub fn is_empty(&self) -> bool {
        self.entries.is_empty()
    }

    pub fn entries(&self) -> &[SourceEntry] {
        &self.entries
    }
}
