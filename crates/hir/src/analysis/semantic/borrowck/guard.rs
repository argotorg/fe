use std::collections::{BTreeMap, BTreeSet};

use crate::analysis::semantic::{SLocalId, VariantIndex};

use super::shape::{SlotPath, SlotProjection};

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct IndexParamId(pub u32);

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ResultIndexId(pub u32);

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ValueIndexId(pub u32);

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ExistentialId(pub u32);

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum IndexExpr {
    Const(usize),
    Runtime(SLocalId),
    ValueParam(ValueIndexId),
    LoanParam(IndexParamId),
    ResultParam(ResultIndexId),
    InputParam(u32),
    Existential(ExistentialId),
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub(crate) enum ValueScope {
    Relative,
    Local(SLocalId),
    Argument(u32),
    Summary,
}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub(crate) struct ChoiceKey {
    scope: ValueScope,
    occurrence: SlotPath<IndexExpr>,
}

impl ChoiceKey {
    pub(crate) fn relative(occurrence: SlotPath<IndexExpr>) -> Self {
        Self {
            scope: ValueScope::Relative,
            occurrence,
        }
    }

    fn substitute(&self, subst: &IndexSubst) -> Self {
        Self {
            scope: self.scope,
            occurrence: substitute_slot_path(&self.occurrence, subst),
        }
    }

    pub(crate) fn scoped(&self, scope: ValueScope) -> Self {
        Self {
            scope: match self.scope {
                ValueScope::Relative => scope,
                scope => scope,
            },
            occurrence: self.occurrence.clone(),
        }
    }
}

#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub(crate) struct IndexSubst {
    entries: BTreeMap<IndexExpr, IndexExpr>,
}

impl IndexSubst {
    pub(crate) fn new() -> Self {
        Self::default()
    }

    pub(crate) fn from_pair(from: IndexExpr, to: IndexExpr) -> Self {
        let mut subst = Self::new();
        subst.insert(from, to);
        subst
    }

    pub(crate) fn insert(&mut self, from: IndexExpr, to: IndexExpr) {
        if from == to {
            self.entries.remove(&from);
        } else {
            self.entries.insert(from, to);
        }
    }

    pub(crate) fn apply(&self, expr: IndexExpr) -> IndexExpr {
        let mut current = expr;
        let mut seen = BTreeSet::new();
        while let Some(next) = self.entries.get(&current).copied() {
            if !seen.insert(current) {
                return seen.into_iter().min().unwrap_or(current);
            }
            current = next;
        }
        current
    }

    pub(crate) fn is_empty(&self) -> bool {
        self.entries.is_empty()
    }
}

#[derive(Clone, Debug, Default)]
struct IndexAtoms {
    equalities: Vec<(IndexExpr, IndexExpr)>,
    disequalities: Vec<(IndexExpr, IndexExpr)>,
    bounds: Vec<(IndexExpr, usize)>,
}

#[derive(Clone, Debug, Default, PartialEq, Eq, PartialOrd, Ord, Hash)]
struct IndexConstraints {
    classes: Box<[Box<[IndexExpr]>]>,
    disequalities: Box<[(IndexExpr, IndexExpr)]>,
    bounds: Box<[(IndexExpr, usize)]>,
}

impl IndexConstraints {
    fn from_atoms(atoms: IndexAtoms) -> Option<Self> {
        let mut terms = BTreeSet::new();
        for (lhs, rhs) in atoms.equalities.iter().chain(atoms.disequalities.iter()) {
            terms.insert(*lhs);
            terms.insert(*rhs);
        }
        terms.extend(atoms.bounds.iter().map(|(expr, _)| *expr));
        let terms = terms.into_iter().collect::<Vec<_>>();
        let indices = terms
            .iter()
            .copied()
            .enumerate()
            .map(|(idx, expr)| (expr, idx))
            .collect::<BTreeMap<_, _>>();
        let mut parents = (0..terms.len()).collect::<Vec<_>>();

        for (lhs, rhs) in &atoms.equalities {
            let lhs = find_root(&mut parents, indices[lhs]);
            let rhs = find_root(&mut parents, indices[rhs]);
            if lhs != rhs {
                let (root, child) = if lhs < rhs { (lhs, rhs) } else { (rhs, lhs) };
                parents[child] = root;
            }
        }

        let mut grouped = BTreeMap::<usize, Vec<IndexExpr>>::new();
        for (idx, expr) in terms.iter().copied().enumerate() {
            let root = find_root(&mut parents, idx);
            grouped.entry(root).or_default().push(expr);
        }
        let mut representative = BTreeMap::new();
        let mut classes = Vec::new();
        for mut members in grouped.into_values() {
            members.sort_unstable();
            let constants = members
                .iter()
                .filter_map(|expr| match expr {
                    IndexExpr::Const(value) => Some(*value),
                    _ => None,
                })
                .collect::<BTreeSet<_>>();
            if constants.len() > 1 {
                return None;
            }
            let key = constants
                .first()
                .copied()
                .map(IndexExpr::Const)
                .unwrap_or(members[0]);
            for member in &members {
                representative.insert(*member, key);
            }
            if members.len() > 1 {
                classes.push(members.into_boxed_slice());
            }
        }
        classes.sort_unstable_by_key(|class| class_key(class));

        let mut disequalities = BTreeSet::new();
        for (lhs, rhs) in atoms.disequalities {
            let lhs = representative.get(&lhs).copied().unwrap_or(lhs);
            let rhs = representative.get(&rhs).copied().unwrap_or(rhs);
            if lhs == rhs {
                return None;
            }
            if matches!((lhs, rhs), (IndexExpr::Const(_), IndexExpr::Const(_))) {
                continue;
            }
            disequalities.insert(ordered_pair(lhs, rhs));
        }

        let mut bounds = BTreeMap::<IndexExpr, usize>::new();
        for (expr, len) in atoms.bounds {
            let expr = representative.get(&expr).copied().unwrap_or(expr);
            if let IndexExpr::Const(value) = expr {
                if value >= len {
                    return None;
                }
                continue;
            }
            bounds
                .entry(expr)
                .and_modify(|bound| *bound = (*bound).min(len))
                .or_insert(len);
        }

        if bounds.iter().any(|(expr, len)| {
            disequalities
                .iter()
                .filter_map(|(lhs, rhs)| match (*lhs, *rhs) {
                    (candidate, IndexExpr::Const(value)) | (IndexExpr::Const(value), candidate)
                        if candidate == *expr && value < *len =>
                    {
                        Some(value)
                    }
                    _ => None,
                })
                .count()
                >= *len
        }) {
            return None;
        }

        Some(Self {
            classes: classes.into_boxed_slice(),
            disequalities: disequalities.into_iter().collect(),
            bounds: bounds.into_iter().collect(),
        })
    }

    fn atoms(&self) -> IndexAtoms {
        let equalities =
            self.classes
                .iter()
                .flat_map(|class| {
                    class.first().copied().into_iter().flat_map(|first| {
                        class.iter().skip(1).copied().map(move |term| (first, term))
                    })
                })
                .collect();
        IndexAtoms {
            equalities,
            disequalities: self.disequalities.to_vec(),
            bounds: self.bounds.to_vec(),
        }
    }

    fn and(&self, other: &Self) -> Option<Self> {
        let mut atoms = self.atoms();
        let other = other.atoms();
        atoms.equalities.extend(other.equalities);
        atoms.disequalities.extend(other.disequalities);
        atoms.bounds.extend(other.bounds);
        Self::from_atoms(atoms)
    }

    fn substitute(&self, subst: &IndexSubst) -> Option<Self> {
        if subst.is_empty() {
            return Some(self.clone());
        }
        let mut atoms = self.atoms();
        for (lhs, rhs) in &mut atoms.equalities {
            *lhs = subst.apply(*lhs);
            *rhs = subst.apply(*rhs);
        }
        for (lhs, rhs) in &mut atoms.disequalities {
            *lhs = subst.apply(*lhs);
            *rhs = subst.apply(*rhs);
        }
        for (expr, _) in &mut atoms.bounds {
            *expr = subst.apply(*expr);
        }
        Self::from_atoms(atoms)
    }

    fn implies(&self, other: &Self) -> bool {
        other.classes.iter().all(|class| {
            class.first().is_none_or(|first| {
                class
                    .iter()
                    .skip(1)
                    .all(|term| self.proves_equal(*first, *term))
            })
        }) && other
            .disequalities
            .iter()
            .all(|(lhs, rhs)| self.proves_disequal(*lhs, *rhs))
            && other
                .bounds
                .iter()
                .all(|(expr, len)| match self.canonical_term(*expr) {
                    IndexExpr::Const(value) => value < *len,
                    expr => self
                        .bounds
                        .iter()
                        .find_map(|(candidate, bound)| (*candidate == expr).then_some(*bound))
                        .is_some_and(|bound| bound <= *len),
                })
    }

    fn proves_equal(&self, lhs: IndexExpr, rhs: IndexExpr) -> bool {
        lhs == rhs || self.canonical_term(lhs) == self.canonical_term(rhs)
    }

    fn proves_disequal(&self, lhs: IndexExpr, rhs: IndexExpr) -> bool {
        let lhs = self.canonical_term(lhs);
        let rhs = self.canonical_term(rhs);
        matches!((lhs, rhs), (IndexExpr::Const(lhs), IndexExpr::Const(rhs)) if lhs != rhs)
            || self
                .disequalities
                .binary_search(&ordered_pair(lhs, rhs))
                .is_ok()
    }

    fn canonical_term(&self, expr: IndexExpr) -> IndexExpr {
        self.classes
            .iter()
            .find(|class| class.binary_search(&expr).is_ok())
            .map_or(expr, |class| class_key(class))
    }
}

fn find_root(parents: &mut [usize], mut index: usize) -> usize {
    while parents[index] != index {
        let parent = parents[index];
        parents[index] = parents[parent];
        index = parents[index];
    }
    index
}

fn class_key(class: &[IndexExpr]) -> IndexExpr {
    class
        .iter()
        .find(|expr| matches!(expr, IndexExpr::Const(_)))
        .copied()
        .unwrap_or(class[0])
}

fn ordered_pair(lhs: IndexExpr, rhs: IndexExpr) -> (IndexExpr, IndexExpr) {
    if lhs <= rhs { (lhs, rhs) } else { (rhs, lhs) }
}

#[derive(Clone, Debug, Default, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Guard {
    index: IndexConstraints,
    variants: Box<[(ChoiceKey, VariantIndex)]>,
}

impl Guard {
    pub(crate) fn always() -> Self {
        Self::default()
    }

    pub(crate) fn equal(lhs: IndexExpr, rhs: IndexExpr) -> Option<Self> {
        Self::always().with_equality(lhs, rhs)
    }

    pub(crate) fn not_equal(lhs: IndexExpr, rhs: IndexExpr) -> Option<Self> {
        Self::always().with_disequality(lhs, rhs)
    }

    pub(crate) fn bounded(expr: IndexExpr, len: usize) -> Option<Self> {
        Self::always().with_bound(expr, len)
    }

    pub(crate) fn and(&self, other: &Self) -> Option<Self> {
        let index = self.index.and(&other.index)?;
        let mut variants = self.variants.iter().cloned().collect::<BTreeMap<_, _>>();
        for (choice, variant) in &other.variants {
            if variants
                .insert(choice.clone(), *variant)
                .is_some_and(|existing| existing != *variant)
            {
                return None;
            }
        }
        Some(Self {
            index,
            variants: variants.into_iter().collect(),
        })
    }

    pub(crate) fn with_equality(&self, lhs: IndexExpr, rhs: IndexExpr) -> Option<Self> {
        self.and(&Self::from_atoms(IndexAtoms {
            equalities: vec![(lhs, rhs)],
            ..IndexAtoms::default()
        })?)
    }

    pub(crate) fn with_disequality(&self, lhs: IndexExpr, rhs: IndexExpr) -> Option<Self> {
        self.and(&Self::from_atoms(IndexAtoms {
            disequalities: vec![(lhs, rhs)],
            ..IndexAtoms::default()
        })?)
    }

    pub(crate) fn with_bound(&self, expr: IndexExpr, len: usize) -> Option<Self> {
        self.and(&Self::from_atoms(IndexAtoms {
            bounds: vec![(expr, len)],
            ..IndexAtoms::default()
        })?)
    }

    pub(crate) fn with_variant(&self, choice: ChoiceKey, variant: VariantIndex) -> Option<Self> {
        self.and(&Self {
            index: IndexConstraints::default(),
            variants: vec![(choice, variant)].into_boxed_slice(),
        })
    }

    pub(crate) fn substitute(&self, subst: &IndexSubst) -> Option<Self> {
        let index = self.index.substitute(subst)?;
        let variants = self
            .variants
            .iter()
            .map(|(choice, variant)| (choice.substitute(subst), *variant))
            .collect::<BTreeMap<_, _>>();
        Some(Self {
            index,
            variants: variants.into_iter().collect(),
        })
    }

    pub(crate) fn scoped(&self, scope: ValueScope) -> Self {
        Self {
            index: self.index.clone(),
            variants: self
                .variants
                .iter()
                .map(|(choice, variant)| (choice.scoped(scope), *variant))
                .collect(),
        }
    }

    pub(crate) fn implies(&self, other: &Self) -> bool {
        self.index.implies(&other.index)
            && other
                .variants
                .iter()
                .all(|expected| self.variants.binary_search(expected).is_ok())
    }

    pub(crate) fn satisfiable(&self) -> bool {
        true
    }

    pub(crate) fn proves_equal(&self, lhs: IndexExpr, rhs: IndexExpr) -> bool {
        self.index.proves_equal(lhs, rhs)
    }

    pub(crate) fn existential_ids(&self) -> BTreeSet<ExistentialId> {
        let mut existentials = BTreeSet::new();
        let atoms = self.index.atoms();
        for expr in atoms
            .equalities
            .iter()
            .chain(&atoms.disequalities)
            .flat_map(|(lhs, rhs)| [lhs, rhs])
            .chain(atoms.bounds.iter().map(|(expr, _)| expr))
        {
            if let IndexExpr::Existential(id) = expr {
                existentials.insert(*id);
            }
        }
        for (choice, _) in &self.variants {
            for projection in choice.occurrence.as_slice() {
                if let SlotProjection::Index(IndexExpr::Existential(id)) = projection {
                    existentials.insert(*id);
                }
            }
        }
        existentials
    }

    pub(crate) fn index_exprs(&self) -> BTreeSet<IndexExpr> {
        let atoms = self.index.atoms();
        let mut expressions = atoms
            .equalities
            .iter()
            .chain(&atoms.disequalities)
            .flat_map(|(lhs, rhs)| [*lhs, *rhs])
            .chain(atoms.bounds.iter().map(|(expr, _)| *expr))
            .collect::<BTreeSet<_>>();
        expressions.extend(self.variants.iter().flat_map(|(choice, _)| {
            choice
                .occurrence
                .as_slice()
                .iter()
                .filter_map(|projection| match projection {
                    SlotProjection::Index(index) => Some(*index),
                    SlotProjection::Field(_) | SlotProjection::VariantField { .. } => None,
                })
        }));
        expressions
    }

    #[cfg(test)]
    pub(crate) fn alpha_normalize_existentials(&self) -> Self {
        let mut subst = IndexSubst::new();
        for (next, old) in self.existential_ids().into_iter().enumerate() {
            subst.insert(
                IndexExpr::Existential(old),
                IndexExpr::Existential(ExistentialId(next as u32)),
            );
        }
        self.substitute(&subst)
            .expect("alpha-renaming preserves guard satisfiability")
    }

    fn from_atoms(atoms: IndexAtoms) -> Option<Self> {
        Some(Self {
            index: IndexConstraints::from_atoms(atoms)?,
            variants: Box::new([]),
        })
    }
}

fn substitute_slot_path(path: &SlotPath<IndexExpr>, subst: &IndexSubst) -> SlotPath<IndexExpr> {
    SlotPath::from_steps(path.as_slice().iter().map(|projection| match projection {
        SlotProjection::Field(field) => SlotProjection::Field(*field),
        SlotProjection::VariantField { variant, field } => SlotProjection::VariantField {
            variant: *variant,
            field: *field,
        },
        SlotProjection::Index(index) => SlotProjection::Index(subst.apply(*index)),
    }))
}

#[cfg(test)]
mod tests {
    use super::*;

    fn runtime(index: u32) -> IndexExpr {
        IndexExpr::Runtime(SLocalId::from_u32(index))
    }

    #[test]
    fn equality_and_disequality_detect_contradictions() {
        let guard = Guard::equal(runtime(0), IndexExpr::Const(1)).expect("valid equality");

        assert!(
            guard
                .with_disequality(runtime(0), IndexExpr::Const(1))
                .is_none()
        );
        assert!(Guard::equal(IndexExpr::Const(1), IndexExpr::Const(2)).is_none());
        assert!(Guard::not_equal(IndexExpr::Const(1), IndexExpr::Const(2)).is_some());
    }

    #[test]
    fn bounds_follow_equalities_and_substitutions() {
        let guard = Guard::equal(runtime(0), runtime(1))
            .expect("valid equality")
            .with_bound(runtime(0), 2)
            .expect("valid bound");
        let expected = Guard::bounded(runtime(1), 3).expect("valid bound");
        assert!(guard.implies(&expected));

        let subst = IndexSubst::from_pair(runtime(0), IndexExpr::Const(3));
        assert!(guard.substitute(&subst).is_none());
    }

    #[test]
    fn enum_choices_are_scoped_and_exclusive() {
        let choice = ChoiceKey::relative(SlotPath::new());
        let guard = Guard::always()
            .with_variant(choice.clone(), VariantIndex(0))
            .expect("first variant");

        assert!(guard.with_variant(choice, VariantIndex(1)).is_none());
        assert!(
            guard
                .scoped(ValueScope::Argument(0))
                .and(&guard.scoped(ValueScope::Argument(1)))
                .is_some()
        );
    }

    #[test]
    fn existential_alpha_renaming_is_stable() {
        let left = Guard::equal(
            IndexExpr::Existential(ExistentialId(9)),
            IndexExpr::Const(1),
        )
        .expect("valid guard");
        let right = Guard::equal(
            IndexExpr::Existential(ExistentialId(3)),
            IndexExpr::Const(1),
        )
        .expect("valid guard");

        assert_eq!(
            left.alpha_normalize_existentials(),
            right.alpha_normalize_existentials()
        );
    }
}
