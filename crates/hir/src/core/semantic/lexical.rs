//! Owned lexical frame lifecycle, independent of each payload's lookup policy.

/// A nonempty chain whose root survives leaving the outermost scope.
/// Consumers own insertion and matching; this owner supplies lexical order.
#[derive(Clone)]
pub(crate) struct LexicalFrames<Data> {
    frames: Vec<Data>,
}

impl<Data> LexicalFrames<Data> {
    pub(crate) fn new(root: Data) -> Self {
        Self { frames: vec![root] }
    }

    pub(crate) fn enter(&mut self, frame: Data) {
        self.frames.push(frame);
    }

    pub(crate) fn leave(&mut self) {
        if self.frames.len() > 1 {
            self.frames.pop();
        }
    }

    pub(crate) fn current_mut(&mut self) -> &mut Data {
        self.frames.last_mut().expect("lexical chain has a root")
    }

    pub(crate) fn inner_first(&self) -> impl Iterator<Item = &Data> {
        self.frames.iter().rev()
    }
}
