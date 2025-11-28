use common::ingot::Ingot;
use rustc_hash::{FxHashMap, FxHashSet};

use crate::analysis::ty::adt_def::AdtRef;
use crate::analysis::ty::ty_def::TyId;
use crate::{
    analysis::HirAnalysisDb,
    core::adt_lower::lower_adt,
    hir_def::{
        CallableDef, Func, HirIngot, ItemKind, TopLevelMod,
        attr::{Attr, AttrArgValue, NormalAttr},
    },
};
use common::ingot::IngotKind;

/// Well-known compiler-provided helpers that can be tagged with `#[lang = "..."]`.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum LangItem {
    Alloc,
    GetField,
    StoreField,
    StoreDiscriminant,
    GetDiscriminant,
    StoreVariantField,
    GetVariantField,
    AddressSpace,
}

impl LangItem {
    fn from_key(key: &str) -> Option<Self> {
        match key {
            "alloc" => Some(Self::Alloc),
            "get_field" => Some(Self::GetField),
            "store_field" => Some(Self::StoreField),
            "store_discriminant" => Some(Self::StoreDiscriminant),
            "get_discriminant" => Some(Self::GetDiscriminant),
            "store_variant_field" => Some(Self::StoreVariantField),
            "get_variant_field" => Some(Self::GetVariantField),
            "address_space" => Some(Self::AddressSpace),
            _ => None,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum LangItemTarget<'db> {
    Callable(CallableDef<'db>),
    Type(TyId<'db>),
}

/// Registry of all lang items visible from an ingot (including its dependencies).
#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct LangItemTable<'db> {
    items: FxHashMap<LangItem, LangItemTarget<'db>>,
}

impl<'db> LangItemTable<'db> {
    fn insert(&mut self, item: LangItem, target: LangItemTarget<'db>) {
        self.items.entry(item).or_insert(target);
    }

    pub fn get_callable(&self, item: LangItem) -> Option<CallableDef<'db>> {
        match self.items.get(&item)? {
            LangItemTarget::Callable(def) => Some(*def),
            _ => None,
        }
    }

    pub fn get_type(&self, item: LangItem) -> Option<TyId<'db>> {
        match self.items.get(&item)? {
            LangItemTarget::Type(ty) => Some(*ty),
            _ => None,
        }
    }
}

/// Collects lang items reachable from `ingot`, preferring items in `ingot` over its dependencies.
pub fn lang_items<'db>(db: &'db dyn HirAnalysisDb, ingot: Ingot<'db>) -> LangItemTable<'db> {
    let mut visited = FxHashSet::default();
    let mut table = LangItemTable::default();
    collect_ingot_lang_items(db, ingot, &mut visited, &mut table);
    table
}

fn collect_ingot_lang_items<'db>(
    db: &'db dyn HirAnalysisDb,
    ingot: Ingot<'db>,
    visited: &mut FxHashSet<Ingot<'db>>,
    table: &mut LangItemTable<'db>,
) {
    if !visited.insert(ingot) {
        return;
    }

    collect_from_ingot(db, ingot, table);

    for (_, dep_url) in ingot.dependencies(db) {
        if let Some(dep_ingot) = db.workspace().containing_ingot(db, dep_url.clone()) {
            collect_ingot_lang_items(db, dep_ingot, visited, table);
        }
    }
}

fn collect_from_ingot<'db>(
    db: &'db dyn HirAnalysisDb,
    ingot: Ingot<'db>,
    table: &mut LangItemTable<'db>,
) {
    let modules: Vec<TopLevelMod<'db>> = ingot.module_tree(db).all_modules().collect();
    for module in modules {
        for &item in module.all_items(db).iter() {
            collect_from_item(db, item, table);
        }
    }
}

fn collect_from_item<'db>(
    db: &'db dyn HirAnalysisDb,
    item: ItemKind<'db>,
    table: &mut LangItemTable<'db>,
) {
    let attrs = match item.attrs(db) {
        Some(attrs) => attrs,
        None => return,
    };

    for attr in attrs.data(db) {
        let Attr::Normal(normal) = attr else {
            continue;
        };
        let Some(key) = lang_attr_key(db, normal) else {
            continue;
        };
        let Some(lang_item) = LangItem::from_key(&key) else {
            continue;
        };

        match item {
            ItemKind::Func(func) => record_callable(db, func, lang_item, table),
            ItemKind::Enum(enum_) if lang_item == LangItem::AddressSpace => {
                record_type(db, lang_item, AdtRef::from(enum_), table)
            }
            ItemKind::Struct(struct_) if lang_item == LangItem::AddressSpace => {
                record_type(db, lang_item, AdtRef::from(struct_), table)
            }
            // Ignore other item kinds for now (e.g., types).
            _ => {}
        }
    }
}

fn record_callable<'db>(
    db: &'db dyn HirAnalysisDb,
    func: Func<'db>,
    item: LangItem,
    table: &mut LangItemTable<'db>,
) {
    // Only consider lang items declared in core or the current ingot.
    let kind = func.top_mod(db).ingot(db).kind(db);
    if !matches!(
        kind,
        IngotKind::Core | IngotKind::Local | IngotKind::StandAlone
    ) {
        return;
    }
    if let Some(def) = func.as_callable(db) {
        table.insert(item, LangItemTarget::Callable(def));
    }
}

fn record_type<'db>(
    db: &'db dyn HirAnalysisDb,
    item: LangItem,
    adt: AdtRef<'db>,
    table: &mut LangItemTable<'db>,
) {
    let ingot_kind = match adt {
        AdtRef::Enum(e) => e.top_mod(db).ingot(db).kind(db),
        AdtRef::Struct(s) => s.top_mod(db).ingot(db).kind(db),
        AdtRef::Contract(c) => c.top_mod(db).ingot(db).kind(db),
    };
    if !matches!(
        ingot_kind,
        IngotKind::Core | IngotKind::Local | IngotKind::StandAlone
    ) {
        return;
    }
    // Lowering ADTs is cheap and cached via salsa.
    let adt_def = lower_adt(db, adt);
    let ty = TyId::adt(db, adt_def);
    table.insert(item, LangItemTarget::Type(ty));
}

fn lang_attr_key<'db>(db: &'db dyn HirAnalysisDb, attr: &NormalAttr<'db>) -> Option<String> {
    let path = attr.path.to_opt()?;
    let ident = path.as_ident(db)?;
    if ident.data(db) != "lang" {
        return None;
    }

    if let Some(value) = attr.value.clone().to_opt()
        && let Some(text) = attr_value_to_string(db, value) {
            return Some(text);
        }

    for arg in &attr.args {
        if let Some(value) = arg.value.clone().to_opt()
            && let Some(text) = attr_value_to_string(db, value) {
                return Some(text);
            }
        if let Some(key_path) = arg.key.to_opt()
            && let Some(key_ident) = key_path.as_ident(db)
        {
            return Some(key_ident.data(db).to_string());
        }
    }

    None
}

fn attr_value_to_string<'db>(
    db: &'db dyn HirAnalysisDb,
    value: AttrArgValue<'db>,
) -> Option<String> {
    match value {
        AttrArgValue::Ident(id) => Some(id.data(db).to_string()),
        AttrArgValue::Lit(lit) => match lit {
            crate::hir_def::LitKind::String(s) => Some(s.data(db).to_string()),
            crate::hir_def::LitKind::Int(i) => Some(i.data(db).to_string()),
            crate::hir_def::LitKind::Bool(b) => Some(b.to_string()),
        },
    }
}
