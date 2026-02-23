use std::io;

use common::InputDb;
use common::ingot::Ingot;
use hir::{
    core::semantic::{ReferenceIndex, SymbolView},
    hir_def::ItemKind,
};

/// Byte-offset index of line starts in a text string.
pub(crate) struct LineIndex {
    offsets: Vec<usize>,
}

/// A resolved position within a `LineIndex`.
pub(crate) struct LinePosition {
    pub line: usize,
    pub line_start_offset: usize,
    pub byte_offset: usize,
}

impl LineIndex {
    pub fn new(text: &str) -> Self {
        let mut offsets = vec![0];
        for (i, b) in text.bytes().enumerate() {
            if b == b'\n' {
                offsets.push(i + 1);
            }
        }
        Self { offsets }
    }

    pub fn position(&self, byte_offset: usize) -> LinePosition {
        let line = self
            .offsets
            .partition_point(|&line_start| line_start <= byte_offset)
            .saturating_sub(1);
        LinePosition {
            line,
            line_start_offset: self.offsets[line],
            byte_offset,
        }
    }
}

/// Shared ingot resolution context used by both SCIP and LSIF generators.
pub(crate) struct IngotContext<'db> {
    pub ingot: Ingot<'db>,
    pub name: String,
    pub version: String,
    pub ref_index: ReferenceIndex<'db>,
}

impl<'db> IngotContext<'db> {
    pub fn resolve(
        db: &'db driver::DriverDataBase,
        ingot_url: &url::Url,
    ) -> io::Result<Self> {
        let Some(ingot) = db.workspace().containing_ingot(db, ingot_url.clone()) else {
            return Err(io::Error::new(
                io::ErrorKind::NotFound,
                "Could not resolve ingot",
            ));
        };

        let name = ingot
            .config(db)
            .and_then(|c| c.metadata.name)
            .map(|n| n.to_string())
            .unwrap_or_else(|| "unknown".to_string());
        let version = ingot
            .version(db)
            .map(|v| v.to_string())
            .unwrap_or_else(|| "0.0.0".to_string());

        let ref_index = ReferenceIndex::build(db, ingot);

        Ok(Self {
            ingot,
            name,
            version,
            ref_index,
        })
    }
}

/// Signature and docstring extracted from a SymbolView.
pub(crate) struct SymbolDocs {
    pub signature: Option<String>,
    pub docstring: Option<String>,
}

pub(crate) fn item_docs(db: &driver::DriverDataBase, item: ItemKind) -> SymbolDocs {
    let sym = SymbolView::from_item(item);
    SymbolDocs {
        signature: sym.signature(db),
        docstring: sym.docs(db),
    }
}

/// Structured hover documentation, ready for format-specific rendering.
pub(crate) struct HoverParts {
    pub signature: Option<String>,
    pub docstring: Option<String>,
}

/// Build hover documentation parts for an item.
pub(crate) fn hover_parts(db: &driver::DriverDataBase, item: ItemKind) -> HoverParts {
    let docs = item_docs(db, item);
    HoverParts {
        signature: docs.signature,
        docstring: docs.docstring,
    }
}

impl HoverParts {
    /// Format for SCIP: signature in ` ```fe ` fence, joined with docstring.
    pub fn to_scip_documentation(&self) -> Vec<String> {
        let mut parts = Vec::new();
        if let Some(sig) = &self.signature {
            parts.push(format!("```fe\n{sig}\n```"));
        }
        if let Some(doc) = &self.docstring {
            parts.push(doc.clone());
        }
        if parts.is_empty() {
            Vec::new()
        } else {
            vec![parts.join("\n\n")]
        }
    }

    /// Format for LSIF: raw signature + docstring.
    pub fn to_lsif_hover(&self) -> Option<String> {
        let sig = self.signature.as_ref()?;
        if let Some(doc) = &self.docstring {
            Some(format!("{sig}\n\n{doc}"))
        } else {
            Some(sig.clone())
        }
    }
}
