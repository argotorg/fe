//! Resource domains: the storage effects confer authority over.
//!
//! Each provider source (a contract field, an effect parameter of the body, a
//! root provider) is a domain, and a handle whose provider is unknown names a
//! dynamic domain. The storage layout makes distinct contract fields
//! disjoint; any other two domains of one address space may share storage,
//! since a caller may supply overlapping providers to distinct effects.
use crate::{
    analysis::ty::provider::AddressSpace,
    semantic::{ProviderBinding, ProviderSource},
};

#[derive(Clone, Debug)]
struct Domain<'db> {
    /// The provider the domain is, or `None` for a dynamic domain.
    source: Option<ProviderSource<'db>>,
    space: Option<AddressSpace>,
}

#[derive(Clone, Debug, Default)]
pub(super) struct Domains<'db>(Vec<Domain<'db>>);

impl<'db> Domains<'db> {
    fn intern(&mut self, domain: Domain<'db>) -> u32 {
        let index = self
            .0
            .iter()
            .position(|known| {
                known.source == domain.source
                    && (domain.source.is_some() || known.space == domain.space)
            })
            .unwrap_or_else(|| {
                self.0.push(domain);
                self.0.len() - 1
            });
        index as u32
    }

    pub fn provider(&mut self, binding: &ProviderBinding<'db>) -> u32 {
        self.provider_source(binding.source.clone(), binding.semantics.address_space)
    }

    pub fn provider_source(
        &mut self,
        source: ProviderSource<'db>,
        space: Option<AddressSpace>,
    ) -> u32 {
        self.intern(Domain {
            source: Some(source),
            space,
        })
    }

    pub fn dynamic(&mut self, space: Option<AddressSpace>) -> u32 {
        self.intern(Domain {
            source: None,
            space,
        })
    }

    pub fn space(&self, domain: u32) -> Option<AddressSpace> {
        self.0[domain as usize].space
    }

    /// Whether two distinct domains may share storage when their places lie
    /// in one space.
    pub fn may_alias(&self, lhs: u32, rhs: u32) -> bool {
        !matches!(
            (&self.0[lhs as usize].source, &self.0[rhs as usize].source),
            (
                Some(ProviderSource::ContractField { .. }),
                Some(ProviderSource::ContractField { .. })
            )
        )
    }
}
