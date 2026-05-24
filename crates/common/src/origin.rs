/// Owner-aware identity for origin nodes whose local IDs are scoped.
///
/// The fields are intentionally private: callers must provide an owner and a
/// local ID together, so a body-local ID cannot masquerade as a global origin.
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct OriginKey<Owner, Local> {
    owner: Owner,
    local: Local,
}

// SAFETY: `OriginKey` is a plain owner/local product. Delegating updates to the
// field implementations preserves Salsa's revision semantics for both parts.
unsafe impl<Owner, Local> salsa::Update for OriginKey<Owner, Local>
where
    Owner: salsa::Update,
    Local: salsa::Update,
{
    unsafe fn maybe_update(old_pointer: *mut Self, new_value: Self) -> bool {
        let mut changed = false;
        unsafe {
            changed |= Owner::maybe_update(&mut (*old_pointer).owner, new_value.owner);
            changed |= Local::maybe_update(&mut (*old_pointer).local, new_value.local);
        }
        changed
    }
}

impl<Owner, Local> OriginKey<Owner, Local> {
    pub const fn new(owner: Owner, local: Local) -> Self {
        Self { owner, local }
    }

    pub fn owner(&self) -> &Owner {
        &self.owner
    }

    pub fn local(&self) -> &Local {
        &self.local
    }

    pub fn into_parts(self) -> (Owner, Local) {
        (self.owner, self.local)
    }
}
