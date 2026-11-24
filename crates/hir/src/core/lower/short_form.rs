use super::{FileLowerCtxt, hir_builder::HirBuilder};
use crate::{
    hir_def::{
        AssocConstDef, ImplTrait, ImplTraitModifiers, Partial, TrackedItemVariant, TraitRefId,
        TypeId,
    },
    span::DesugaredOrigin,
};

/// Lower one ordinary trait derive header implied by a declaration. The
/// declaration's range and slot identify its output in each prepared view.
pub(super) fn lower_derive_slot<'db, O: Clone + Into<DesugaredOrigin>>(
    builder: &mut HirBuilder<'_, 'db, O>,
    self_ty: TypeId<'db>,
    trait_ref: TraitRefId<'db>,
    provider_name: &str,
    (range, slot): (parser::TextRange, u8),
    authored_consts: impl FnOnce(&mut FileLowerCtxt<'db>) -> Vec<AssocConstDef<'db>>,
) {
    let db = builder.db();
    let provider = builder.path_from_root(builder.roots().std, &["derive", provider_name]);
    let top_mod = builder.top_mod();
    let plan = common::generation::file_generation(db, top_mod.file(db))
        .as_ref()
        .and_then(|generation| generation.slots.get(&(range, slot)));
    let idx = builder.ctxt().next_impl_trait_idx();
    builder.with_item_scope(TrackedItemVariant::ImplTrait(idx), |builder, id| {
        let mut types = Vec::new();
        let mut consts = authored_consts(builder.ctxt());
        if let Some(plan) = plan {
            super::item::lower_trait_derive_members(
                builder.ctxt(),
                &plan.code,
                range,
                &mut types,
                &mut consts,
            );
        }
        ImplTrait::new(
            db,
            id,
            Partial::Present(trait_ref),
            Partial::Present(self_ty),
            builder.empty_attrs(),
            builder.empty_generic_params(),
            builder.empty_where_clause(),
            types,
            consts,
            Vec::new(),
            ImplTraitModifiers {
                derive_provider: Some(provider),
                derive_trait_ref: Some(trait_ref),
                short_form_derive_site: Some((range, slot)),
                ..Default::default()
            },
            top_mod,
            builder.origin(),
        )
    });
}
