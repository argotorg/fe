//! References from generated code to authored declarations.
//!
//! Generated code is lowered into the module that receives it, but it names
//! declarations written elsewhere: the record a provider reflected, the trait
//! of a scoped goal, and the producer scope its quotation was written in.
//! Lowering a module never reads another module's lowered HIR, because that
//! module may itself receive code naming this one. An [`AuthoredScope`] names a
//! declaration by its module and the authored syntax it was written as, and
//! ordinary resolution binds it in that module's lowered scope graph when a
//! path or type containing it is resolved.

use common::ingot::Ingot;
use parser::TextRange;

use super::{TopLevelMod, scope_graph::ScopeId};
use crate::HirDb;

#[salsa::interned]
#[derive(Debug)]
pub struct AuthoredScope<'db> {
    #[returns(copy)]
    pub top_mod: TopLevelMod<'db>,
    #[returns(copy)]
    pub locator: AuthoredLocator,
}

/// Where an authored scope was written in its module.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum AuthoredLocator {
    /// The struct, enum or trait declared by the item node at this range.
    Item(TextRange),
    /// The generic parameter at `index` of the implementation header written
    /// at `item`.
    GenericParam { item: TextRange, index: u16 },
    /// The scope a quotation was written in: its innermost enclosing block of
    /// an authored body, else its innermost enclosing `mod`, else the module.
    Producer {
        module: Option<TextRange>,
        block: Option<TextRange>,
    },
}

impl<'db> AuthoredScope<'db> {
    pub fn item(db: &'db dyn HirDb, top_mod: TopLevelMod<'db>, range: TextRange) -> Self {
        Self::new(db, top_mod, AuthoredLocator::Item(range))
    }

    pub fn producer(
        db: &'db dyn HirDb,
        top_mod: TopLevelMod<'db>,
        module: Option<TextRange>,
        block: Option<TextRange>,
    ) -> Self {
        Self::new(db, top_mod, AuthoredLocator::Producer { module, block })
    }

    /// The scope this reference names. Only analysis resolves it: lowering
    /// creates references, so it never waits for the module they name.
    pub fn resolve(self, db: &'db dyn HirDb) -> ScopeId<'db> {
        crate::core::lower::resolve_authored_scope(db, self)
    }

    pub fn ingot(self, db: &'db dyn HirDb) -> Ingot<'db> {
        self.top_mod(db).ingot(db)
    }
}
