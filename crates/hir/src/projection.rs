//! Projection system for navigating through compound data structures.
//!
//! This module provides the core abstraction for describing paths through
//! nested data (struct fields, enum variants, array elements, pointer derefs).
//!
//! # Design Principles
//!
//! - **Pure structural navigation**: Projections describe *how* to navigate data,
//!   not ownership, lifetimes, or address spaces (those are separate concerns).
//! - **Generic over type representation**: Works with HIR types, MIR types, or any
//!   other type representation via the `Ty`, `Var`, and `Idx` type parameters.
//! - **Graph IR compatible**: Can be used as instruction operands (CFG+SSA) or
//!   as edge labels (Sea of Nodes) with the same vocabulary.

use smallvec::SmallVec;

/// Atomic projection step - generic over type representation.
///
/// Each projection step describes one level of navigation into a compound type:
/// - `Field`: Access a struct/tuple field by index
/// - `VariantField`: Access a field within an enum variant (prism + lens)
/// - `Index`: Access an array/slice element
/// - `Deref`: Dereference a pointer
///
/// Type parameters:
/// - `Ty`: The type representation (e.g., `TyId<'db>`)
/// - `Var`: The variant representation (e.g., `EnumVariant<'db>`)
/// - `Idx`: The dynamic index ID type (e.g., `ExprId` in HIR, `ValueId` in MIR)
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Projection<Ty, Var, Idx> {
    /// Struct/tuple field by index.
    Field(usize),

    /// Enum variant field access.
    ///
    /// This combines a "prism" (selecting the variant) with a "lens" (selecting
    /// the field within that variant). The `enum_ty` is needed for type-aware
    /// operations like hashing and alias analysis.
    VariantField {
        /// Which variant we're accessing.
        variant: Var,
        /// The enum type (for type identity in analysis).
        enum_ty: Ty,
        /// Field index within the variant.
        field_idx: usize,
    },

    /// Array/slice index access.
    Index(IndexSource<Idx>),

    /// Pointer dereference.
    Deref,
}

/// Source of an array/slice index.
///
/// Indices can be compile-time constants or runtime values. For runtime values,
/// we store a typed ID that the consumer interprets based on context (could be
/// an ExprId in HIR, a ValueId in MIR, etc.).
///
/// Type parameter `Idx` is the dynamic index ID type, allowing proper
/// placeholder-based hashing in each layer.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum IndexSource<Idx> {
    /// Compile-time constant index.
    Constant(usize),
    /// Runtime index value (typed ID, interpretation depends on context).
    Dynamic(Idx),
}

/// A path through nested data via a sequence of projections.
///
/// Represents a chain of navigation steps from a base value to a nested location.
/// For example, `base.field0.variant_field.1` would be represented as:
/// ```text
/// [Field(0), VariantField { ... }, Field(1)]
/// ```
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ProjectionPath<Ty, Var, Idx>(SmallVec<Projection<Ty, Var, Idx>, 4>);

impl<Ty, Var, Idx> Default for ProjectionPath<Ty, Var, Idx> {
    fn default() -> Self {
        Self(SmallVec::new())
    }
}

impl<Ty, Var, Idx> ProjectionPath<Ty, Var, Idx> {
    /// Create an empty projection path.
    pub fn new() -> Self {
        Self::default()
    }

    /// Create a path from a single projection.
    pub fn from_projection(proj: Projection<Ty, Var, Idx>) -> Self {
        let mut path = Self::new();
        path.push(proj);
        path
    }

    /// Returns true if the path is empty (accesses the base directly).
    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }

    /// Returns the number of projection steps.
    pub fn len(&self) -> usize {
        self.0.len()
    }

    /// Iterate over the projection steps.
    pub fn iter(&self) -> impl Iterator<Item = &Projection<Ty, Var, Idx>> {
        self.0.iter()
    }

    /// Push a projection step onto the path.
    pub fn push(&mut self, proj: Projection<Ty, Var, Idx>) {
        self.0.push(proj);
    }

    /// Pop the last projection step, returning the parent path.
    pub fn parent(&self) -> Option<Self>
    where
        Ty: Clone,
        Var: Clone,
        Idx: Clone,
    {
        if self.0.is_empty() {
            None
        } else {
            let mut steps = self.0.clone();
            steps.pop();
            Some(Self(steps))
        }
    }

    /// Get the last field index if the last projection is a Field or VariantField.
    ///
    /// Returns `None` for Index (even constant), Deref, or empty paths.
    /// Use `last_const_index()` to get constant array indices.
    pub fn last_field_index(&self) -> Option<usize> {
        self.0.last().and_then(|proj| match proj {
            Projection::Field(idx) => Some(*idx),
            Projection::VariantField { field_idx, .. } => Some(*field_idx),
            Projection::Index(_) | Projection::Deref => None,
        })
    }

    /// Get the last constant array index if the last projection is Index(Constant).
    ///
    /// Returns `None` for Field, VariantField, Index(Dynamic), Deref, or empty paths.
    pub fn last_const_index(&self) -> Option<usize> {
        self.0.last().and_then(|proj| match proj {
            Projection::Index(IndexSource::Constant(idx)) => Some(*idx),
            _ => None,
        })
    }
}

impl<Ty: Clone, Var: Clone, Idx: Clone> ProjectionPath<Ty, Var, Idx> {
    /// Concatenate two paths.
    pub fn concat(&self, other: &Self) -> Self {
        let mut result = self.0.clone();
        result.extend(other.0.iter().cloned());
        Self(result)
    }
}

impl<Ty: PartialEq, Var: PartialEq, Idx: PartialEq> ProjectionPath<Ty, Var, Idx> {
    /// Check if `self` is a prefix of `other`.
    ///
    /// Returns true if `other` starts with all the steps in `self`.
    /// An empty path is a prefix of all paths.
    pub fn is_prefix_of(&self, other: &Self) -> bool {
        if self.len() > other.len() {
            return false;
        }
        for (a, b) in self.iter().zip(other.iter()) {
            if a != b {
                return false;
            }
        }
        true
    }

    /// Determine the aliasing relationship between two projection paths.
    ///
    /// This performs structural alias analysis based on the projection steps:
    /// - `Must`: The paths definitely refer to the same location
    /// - `May`: The paths might refer to overlapping locations
    /// - `No`: The paths definitely refer to disjoint locations
    pub fn may_alias(&self, other: &Self) -> Aliasing {
        let mut self_idx = 0;
        let mut other_idx = 0;
        let self_steps: Vec<_> = self.iter().collect();
        let other_steps: Vec<_> = other.iter().collect();

        loop {
            let a = self_steps.get(self_idx);
            let b = other_steps.get(other_idx);

            match (a, b) {
                // Both paths ended at the same point
                (None, None) => return Aliasing::Must,

                // One path is a prefix of the other - they alias
                (None, Some(_)) | (Some(_), None) => return Aliasing::Must,

                // Compare the projections
                (Some(proj_a), Some(proj_b)) => {
                    if proj_a == proj_b {
                        // Same projection step, continue
                        self_idx += 1;
                        other_idx += 1;
                        continue;
                    }

                    // Different projections - check if they could overlap
                    match (*proj_a, *proj_b) {
                        // Different fields are disjoint
                        (Projection::Field(i), Projection::Field(j)) if i != j => {
                            return Aliasing::No;
                        }

                        // Different variant fields are disjoint (different variants)
                        (
                            Projection::VariantField { variant: v1, .. },
                            Projection::VariantField { variant: v2, .. },
                        ) if v1 != v2 => {
                            return Aliasing::No;
                        }

                        // Same variant but different field indices are disjoint
                        (
                            Projection::VariantField {
                                variant: v1,
                                field_idx: f1,
                                ..
                            },
                            Projection::VariantField {
                                variant: v2,
                                field_idx: f2,
                                ..
                            },
                        ) if v1 == v2 && f1 != f2 => {
                            return Aliasing::No;
                        }

                        // Constant indices that differ are disjoint
                        (
                            Projection::Index(IndexSource::Constant(i)),
                            Projection::Index(IndexSource::Constant(j)),
                        ) if i != j => {
                            return Aliasing::No;
                        }

                        // Dynamic indices may alias (we don't know the value)
                        (Projection::Index(_), Projection::Index(_)) => {
                            return Aliasing::May;
                        }

                        // Mixing field/variant access types - shouldn't happen in
                        // well-typed code, but conservatively say May
                        _ => return Aliasing::May,
                    }
                }
            }
        }
    }
}

/// Result of alias analysis between two projection paths.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Aliasing {
    /// The paths definitely refer to the same memory location.
    Must,
    /// The paths might refer to overlapping memory locations.
    May,
    /// The paths definitely refer to disjoint memory locations.
    No,
}

#[cfg(test)]
mod tests {
    use super::*;

    // Simple test types - using u32 for all type parameters
    #[allow(dead_code)]
    type TestProjection = Projection<u32, u32, u32>;
    type TestPath = ProjectionPath<u32, u32, u32>;

    #[test]
    fn test_empty_path() {
        let path = TestPath::new();
        assert!(path.is_empty());
        assert_eq!(path.len(), 0);
    }

    #[test]
    fn test_path_push() {
        let mut path = TestPath::new();
        path.push(Projection::Field(0));
        path.push(Projection::Field(1));
        assert_eq!(path.len(), 2);
    }

    #[test]
    fn test_path_parent() {
        let mut path = TestPath::new();
        path.push(Projection::Field(0));
        path.push(Projection::Field(1));

        let parent = path.parent().unwrap();
        assert_eq!(parent.len(), 1);

        let grandparent = parent.parent().unwrap();
        assert!(grandparent.is_empty());

        assert!(grandparent.parent().is_none());
    }

    #[test]
    fn test_is_prefix_of() {
        let mut path1 = TestPath::new();
        path1.push(Projection::Field(0));

        let mut path2 = TestPath::new();
        path2.push(Projection::Field(0));
        path2.push(Projection::Field(1));

        assert!(path1.is_prefix_of(&path2));
        assert!(!path2.is_prefix_of(&path1));
        assert!(path1.is_prefix_of(&path1));

        let empty = TestPath::new();
        assert!(empty.is_prefix_of(&path1));
        assert!(empty.is_prefix_of(&path2));
    }

    #[test]
    fn test_alias_must() {
        let mut path1 = TestPath::new();
        path1.push(Projection::Field(0));

        let mut path2 = TestPath::new();
        path2.push(Projection::Field(0));

        assert_eq!(path1.may_alias(&path2), Aliasing::Must);
    }

    #[test]
    fn test_alias_no_different_fields() {
        let mut path1 = TestPath::new();
        path1.push(Projection::Field(0));

        let mut path2 = TestPath::new();
        path2.push(Projection::Field(1));

        assert_eq!(path1.may_alias(&path2), Aliasing::No);
    }

    #[test]
    fn test_alias_prefix() {
        let mut path1 = TestPath::new();
        path1.push(Projection::Field(0));

        let mut path2 = TestPath::new();
        path2.push(Projection::Field(0));
        path2.push(Projection::Field(1));

        // path1 accesses .0, path2 accesses .0.1
        // They alias because .0 contains .0.1
        assert_eq!(path1.may_alias(&path2), Aliasing::Must);
    }

    #[test]
    fn test_alias_constant_indices() {
        let mut path1 = TestPath::new();
        path1.push(Projection::Index(IndexSource::Constant(0)));

        let mut path2 = TestPath::new();
        path2.push(Projection::Index(IndexSource::Constant(1)));

        assert_eq!(path1.may_alias(&path2), Aliasing::No);

        let mut path3 = TestPath::new();
        path3.push(Projection::Index(IndexSource::Constant(0)));

        assert_eq!(path1.may_alias(&path3), Aliasing::Must);
    }

    #[test]
    fn test_alias_dynamic_indices() {
        let mut path1 = TestPath::new();
        path1.push(Projection::Index(IndexSource::Dynamic(0)));

        let mut path2 = TestPath::new();
        path2.push(Projection::Index(IndexSource::Dynamic(1)));

        // Dynamic indices may alias (we don't know the runtime values)
        assert_eq!(path1.may_alias(&path2), Aliasing::May);
    }

    #[test]
    fn test_concat() {
        let mut path1 = TestPath::new();
        path1.push(Projection::Field(0));

        let mut path2 = TestPath::new();
        path2.push(Projection::Field(1));

        let combined = path1.concat(&path2);
        assert_eq!(combined.len(), 2);
    }

    #[test]
    fn test_last_field_index() {
        let mut path = TestPath::new();
        assert_eq!(path.last_field_index(), None);

        path.push(Projection::Field(5));
        assert_eq!(path.last_field_index(), Some(5));

        path.push(Projection::VariantField {
            variant: 0,
            enum_ty: 0,
            field_idx: 3,
        });
        assert_eq!(path.last_field_index(), Some(3));

        // Index should return None from last_field_index
        path.push(Projection::Index(IndexSource::Constant(10)));
        assert_eq!(path.last_field_index(), None);

        // But last_const_index should return the value
        assert_eq!(path.last_const_index(), Some(10));
    }

    #[test]
    fn test_last_const_index() {
        let mut path = TestPath::new();
        assert_eq!(path.last_const_index(), None);

        path.push(Projection::Field(5));
        assert_eq!(path.last_const_index(), None);

        path.push(Projection::Index(IndexSource::Constant(7)));
        assert_eq!(path.last_const_index(), Some(7));

        path.push(Projection::Index(IndexSource::Dynamic(99)));
        assert_eq!(path.last_const_index(), None);

        path.push(Projection::Deref);
        assert_eq!(path.last_const_index(), None);
    }
}
