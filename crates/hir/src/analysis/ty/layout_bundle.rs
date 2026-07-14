use salsa::Update;

use super::{
    const_ty::{
        CallableInputLayoutHoleOrigin, LayoutIntroSite, LayoutRootId, StructuralHoleOrigin,
    },
    ty_def::TyId,
};

/// A projection through a type template used to derive layout schemas.
///
/// `ConstParam` selects a type-level root argument rather than a runtime value
/// field. It is therefore intentionally excluded from [`LayoutEvidencePath`].
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Update)]
pub enum LayoutBundlePathStep {
    Field(u16),
    Variant(u16),
    Index,
    ConstParam(u16),
}

pub type LayoutBundlePath = Vec<LayoutBundlePathStep>;

/// A projection through the semantic layout views of a runtime value.
///
/// `EffectTarget` is an opaque view transition, not a physical field. Keeping
/// it explicit prevents a handle's representation roots from aliasing the
/// roots of the logical value reached through `EffectHandle::Target`.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Update)]
pub enum LayoutEvidencePathStep {
    Field(u16),
    Variant(u16),
    Index,
    EffectTarget,
}

pub type LayoutEvidencePath = Vec<LayoutEvidencePathStep>;

/// The declaration-relative position of one layout root inside a const
/// argument. This is deliberately separate from [`LayoutEvidencePath`]: value
/// projection and type-level root selection are different operations.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Update)]
pub struct LayoutRootPort {
    pub param: usize,
    pub ordinal: usize,
}

/// The stable structural identity of one physical layout-map component.
///
/// `value_path` identifies the physical occurrence family in the runtime value
/// shape. `root` identifies the root within the terminal const argument. The
/// instantiated const value is intentionally absent: two components do not
/// become interchangeable merely because specialization gives them equal
/// values.
#[derive(Debug, Clone, PartialEq, Eq, Hash, Update)]
pub struct LayoutPortKey {
    pub value_path: LayoutEvidencePath,
    pub root: LayoutRootPort,
}

/// A finite back-edge in the semantic layout-view graph.
///
/// Every component below `alias` is the same runtime map as the corresponding
/// component below `canonical`. This represents recursive effect-target views
/// without unrolling an infinite family of path-prefixed ABI components.
#[derive(Debug, Clone, PartialEq, Eq, Hash, Update)]
pub struct LayoutViewAlias {
    pub alias: LayoutEvidencePath,
    pub canonical: LayoutEvidencePath,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Update)]
pub struct NonRegularLayoutViewCycle {
    pub canonical: LayoutEvidencePath,
    pub recursive: LayoutEvidencePath,
}

/// One component port in the complete callable input interface.
///
/// [`LayoutPortKey`] is deliberately schema-relative, so an input origin is
/// required whenever a component is named outside its containing schema.
#[derive(Debug, Clone, PartialEq, Eq, Hash, Update)]
pub struct CallableLayoutPort {
    pub origin: CallableInputLayoutHoleOrigin,
    pub component: LayoutPortKey,
}

/// The declaration-relative source of one runtime layout-evidence parameter.
#[derive(Debug, Clone, PartialEq, Eq, Hash, Update)]
pub enum CallableLayoutParamPort {
    Input(CallableLayoutPort),
    OutputWitness(LayoutPortKey),
}

/// The exact semantic type of a layout map.
///
/// Equal rank is insufficient: dimensions determine legal projections and the
/// scalar type determines arithmetic and ABI representation.
#[derive(Debug, Clone, PartialEq, Eq, Hash, Update)]
pub struct LayoutMapTy<'db> {
    pub scalar_ty: TyId<'db>,
    pub dimensions: Vec<usize>,
}

impl<'db> LayoutMapTy<'db> {
    pub fn rank(&self) -> usize {
        self.dimensions.len()
    }

    pub fn projected(&self, axes: usize) -> Option<Self> {
        self.dimensions.get(axes..).map(|dimensions| Self {
            scalar_ty: self.scalar_ty,
            dimensions: dimensions.to_vec(),
        })
    }
}

impl LayoutPortKey {
    pub fn projected(&self, prefix: &[LayoutEvidencePathStep]) -> Option<Self> {
        self.value_path.strip_prefix(prefix).map(|value_path| Self {
            value_path: value_path.to_vec(),
            root: self.root,
        })
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Update)]
pub struct LayoutBundleComponentId(pub u32);

impl LayoutBundleComponentId {
    pub fn index(self) -> usize {
        self.0 as usize
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Update)]
pub enum LayoutBundleComponentKey<'db> {
    Root(LayoutRootId<'db>),
    Param(TyId<'db>),
    Static(TyId<'db>),
}

/// The declaration-level identity of a layout root.
///
/// Structural holes are re-anchored at every callable and semantic-value
/// boundary, so their [`LayoutRootId`] is an instantiated allocation identity,
/// not a stable way to relate compatible components. The source declaration
/// and introduction site survive re-anchoring and specialization. Explicit
/// const parameters and constants already have stable declaration identities.
#[derive(Debug, Clone, PartialEq, Eq, Hash, Update)]
pub enum LayoutBundleComponentDeclaration<'db> {
    Structural {
        origin: StructuralHoleOrigin<'db>,
        introduced_at: LayoutIntroSite<'db>,
    },
    Param(TyId<'db>),
    Static(TyId<'db>),
}

/// Whether a component is part of the runtime layout-evidence ABI.
///
/// This is intentionally independent of [`LayoutBundleComponentKey`]. Generic
/// specialization can give a declaration-level layout parameter a concrete
/// representative while the value's physical occurrence still supplies a
/// different root. Conversely, a concrete component written in the callable's
/// declaration needs no runtime transport.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Update)]
pub enum LayoutBundleTransport {
    CompileTime,
    Runtime,
}

/// One physical runtime-root transport, possibly serving an indexed family.
///
/// A component has exactly one occurrence family. Distinct value paths never
/// share a descriptor merely because generic substitution makes their const
/// arguments equal: value transformations can change either path
/// independently. `port` retains physical occurrence identity,
/// `declaration` relates components through the callable's declared type, and
/// `representative` is an optional instantiated scalar used only for
/// allocation lookup and explicit const resolution. Ranked maps whose members
/// specialize to different scalar values deliberately have no representative.
#[derive(Debug, Clone, PartialEq, Eq, Hash, Update)]
pub struct LayoutBundleComponent<'db> {
    pub id: LayoutBundleComponentId,
    pub port: LayoutPortKey,
    pub declaration: LayoutBundleComponentDeclaration<'db>,
    pub representative: Option<LayoutBundleComponentKey<'db>>,
    pub transport: LayoutBundleTransport,
    pub ty: TyId<'db>,
    /// Declaration-level const parameters whose exact scalar value is
    /// supplied by this port. These identities survive specialization:
    /// instantiated value equality is not evidence provenance.
    pub supplied_const_params: Vec<TyId<'db>>,
    /// Declaration-level const parameters used to compute this port's value.
    /// A dependency is deliberately not a supplied value: `{ROOT + 1}` cannot
    /// reify `ROOT` at runtime.
    pub dependent_const_params: Vec<TyId<'db>>,
    /// Complete enclosing array dimensions for `port.value_path`.
    pub dimensions: Vec<usize>,
}

impl<'db> LayoutBundleComponent<'db> {
    /// Number of runtime axes carried after the component's base.
    ///
    /// Strides are evidence, not derivable schema data: enum overlay can widen
    /// allocator geometry, array repetition introduces a zero stride, and
    /// arbitrary value transformations may require a dense map.
    pub fn rank(&self) -> usize {
        self.dimensions.len()
    }

    pub fn runtime_descriptor_count(&self) -> usize {
        usize::from(self.is_runtime())
    }

    pub fn is_runtime(&self) -> bool {
        matches!(self.transport, LayoutBundleTransport::Runtime)
    }

    pub fn map_ty(&self) -> LayoutMapTy<'db> {
        LayoutMapTy {
            scalar_ty: self.ty,
            dimensions: self.dimensions.clone(),
        }
    }

    /// Whether this component's formal root is available from `candidate`
    /// inside the same callable declaration. Exact declaration identity is
    /// required for explicit const expressions: sharing dependencies does not
    /// make `ROOT` and `ROOT + 1` the same layout value. Formal-parameter
    /// fallback exists only for structural roots re-anchored at a boundary.
    /// Map shape is deliberately ignored because the body may project a
    /// ranked input before returning it.
    pub fn formally_derivable_from(&self, candidate: &Self) -> bool {
        self.declaration == candidate.declaration
            || (matches!(
                self.declaration,
                LayoutBundleComponentDeclaration::Structural { .. }
            ) && !self.supplied_const_params.is_empty()
                && self
                    .supplied_const_params
                    .iter()
                .all(|required| {
                    candidate.supplied_const_params.contains(required)
                        || matches!(candidate.declaration, LayoutBundleComponentDeclaration::Param(value) if value == *required)
                }))
    }

    /// Whether `candidate` can directly supply this exact map value inside the
    /// same callable declaration.
    pub fn formally_supplied_by(&self, candidate: &Self) -> bool {
        self.map_ty() == candidate.map_ty() && self.formally_derivable_from(candidate)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Update, Default)]
pub struct LayoutBundleSchema<'db> {
    pub components: Vec<LayoutBundleComponent<'db>>,
    pub view_aliases: Vec<LayoutViewAlias>,
    pub non_regular_view_cycle: Option<NonRegularLayoutViewCycle>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Update)]
pub enum LayoutBundleSchemaError {
    InvalidComponentId {
        expected: LayoutBundleComponentId,
        actual: LayoutBundleComponentId,
    },
    InvalidOccurrenceRank {
        component: LayoutBundleComponentId,
        path_indices: usize,
        dimensions: usize,
    },
    EmptyDimension {
        component: LayoutBundleComponentId,
        axis: usize,
    },
    InvalidCompileTimeComponent {
        component: LayoutBundleComponentId,
    },
    DuplicateComponent {
        first: LayoutBundleComponentId,
        second: LayoutBundleComponentId,
    },
    DuplicateSuppliedConstParam {
        component: LayoutBundleComponentId,
    },
    DuplicateDependentConstParam {
        component: LayoutBundleComponentId,
    },
    InvalidSuppliedConstParam {
        component: LayoutBundleComponentId,
    },
    NonCanonicalComponent {
        component: LayoutBundleComponentId,
    },
    InvalidViewAlias {
        alias: usize,
    },
    DuplicateViewAlias {
        first: usize,
        second: usize,
    },
    OverlappingViewAlias {
        first: usize,
        second: usize,
    },
    NonRegularViewCycle {
        canonical: LayoutEvidencePath,
        recursive: LayoutEvidencePath,
    },
}

impl LayoutBundleSchema<'_> {
    pub fn validate(&self) -> Result<(), LayoutBundleSchemaError> {
        if let Some(cycle) = &self.non_regular_view_cycle {
            return Err(LayoutBundleSchemaError::NonRegularViewCycle {
                canonical: cycle.canonical.clone(),
                recursive: cycle.recursive.clone(),
            });
        }
        for (idx, alias) in self.view_aliases.iter().enumerate() {
            if alias.alias.len() <= alias.canonical.len()
                || !alias.alias.starts_with(&alias.canonical)
                || alias
                    .alias
                    .iter()
                    .filter(|step| matches!(step, LayoutEvidencePathStep::Index))
                    .count()
                    != alias
                        .canonical
                        .iter()
                        .filter(|step| matches!(step, LayoutEvidencePathStep::Index))
                        .count()
            {
                return Err(LayoutBundleSchemaError::InvalidViewAlias { alias: idx });
            }
            if let Some(first) = self.view_aliases[..idx]
                .iter()
                .position(|other| other.alias == alias.alias)
            {
                return Err(LayoutBundleSchemaError::DuplicateViewAlias { first, second: idx });
            }
            if let Some(first) = self.view_aliases[..idx].iter().position(|other| {
                other.alias.starts_with(&alias.alias) || alias.alias.starts_with(&other.alias)
            }) {
                return Err(LayoutBundleSchemaError::OverlappingViewAlias { first, second: idx });
            }
            if self.canonicalize_view_path(&alias.canonical) != alias.canonical {
                return Err(LayoutBundleSchemaError::InvalidViewAlias { alias: idx });
            }
        }
        for (idx, component) in self.components.iter().enumerate() {
            let expected = LayoutBundleComponentId(idx as u32);
            if component.id != expected {
                return Err(LayoutBundleSchemaError::InvalidComponentId {
                    expected,
                    actual: component.id,
                });
            }
            let path_indices = component
                .port
                .value_path
                .iter()
                .filter(|step| matches!(step, LayoutEvidencePathStep::Index))
                .count();
            if path_indices != component.dimensions.len() {
                return Err(LayoutBundleSchemaError::InvalidOccurrenceRank {
                    component: component.id,
                    path_indices,
                    dimensions: component.dimensions.len(),
                });
            }
            if let Some(axis) = component
                .dimensions
                .iter()
                .position(|dimension| *dimension == 0)
            {
                return Err(LayoutBundleSchemaError::EmptyDimension {
                    component: component.id,
                    axis,
                });
            }
            if !component.is_runtime()
                && !matches!(
                    component.representative,
                    Some(LayoutBundleComponentKey::Static(_))
                )
            {
                return Err(LayoutBundleSchemaError::InvalidCompileTimeComponent {
                    component: component.id,
                });
            }
            if let Some((first, _)) = self.components[..idx]
                .iter()
                .enumerate()
                .find(|(_, other)| other.port == component.port)
            {
                return Err(LayoutBundleSchemaError::DuplicateComponent {
                    first: LayoutBundleComponentId(first as u32),
                    second: component.id,
                });
            }
            if component
                .supplied_const_params
                .iter()
                .enumerate()
                .any(|(idx, param)| component.supplied_const_params[..idx].contains(param))
            {
                return Err(LayoutBundleSchemaError::DuplicateSuppliedConstParam {
                    component: component.id,
                });
            }
            if component
                .dependent_const_params
                .iter()
                .enumerate()
                .any(|(idx, param)| component.dependent_const_params[..idx].contains(param))
            {
                return Err(LayoutBundleSchemaError::DuplicateDependentConstParam {
                    component: component.id,
                });
            }
            if component
                .supplied_const_params
                .iter()
                .any(|param| !component.dependent_const_params.contains(param))
            {
                return Err(LayoutBundleSchemaError::InvalidSuppliedConstParam {
                    component: component.id,
                });
            }
            if self.canonicalize_view_path(&component.port.value_path) != component.port.value_path
            {
                return Err(LayoutBundleSchemaError::NonCanonicalComponent {
                    component: component.id,
                });
            }
        }
        Ok(())
    }

    pub fn canonicalize_view_path(&self, path: &[LayoutEvidencePathStep]) -> LayoutEvidencePath {
        let mut path = path.to_vec();
        loop {
            let Some(alias) = self
                .view_aliases
                .iter()
                .filter(|alias| path.starts_with(&alias.alias))
                .max_by_key(|alias| alias.alias.len())
            else {
                return path;
            };
            let mut canonical = alias.canonical.clone();
            canonical.extend_from_slice(&path[alias.alias.len()..]);
            path = canonical;
        }
    }

    /// Projects one canonical component port through a semantic value view.
    ///
    /// Components physically below `prefix` project directly. A component on
    /// the other side of a recursive view back-edge is first expressed below
    /// that edge, then projected. The shortest expression is canonical for the
    /// selected view and prevents a cycle from manufacturing duplicate ports.
    pub fn projected_port(
        &self,
        port: &LayoutPortKey,
        prefix: &[LayoutEvidencePathStep],
    ) -> Option<LayoutPortKey> {
        let prefix = self.canonicalize_view_path(prefix);
        let direct = port.value_path.strip_prefix(prefix.as_slice());
        let aliased = self
            .view_aliases
            .iter()
            .filter_map(|alias| {
                let suffix = port.value_path.strip_prefix(alias.canonical.as_slice())?;
                let mut path = alias.alias.clone();
                path.extend_from_slice(suffix);
                path.strip_prefix(prefix.as_slice()).map(<[_]>::to_vec)
            })
            .min_by_key(Vec::len);
        direct
            .map(<[_]>::to_vec)
            .or(aliased)
            .map(|value_path| LayoutPortKey {
                value_path,
                root: port.root,
            })
    }

    pub fn canonicalize_port(&self, port: &LayoutPortKey) -> LayoutPortKey {
        LayoutPortKey {
            value_path: self.canonicalize_view_path(&port.value_path),
            root: port.root,
        }
    }

    /// Whether the runtime components in this schema are supplied by one
    /// semantic view of `source`.
    ///
    /// Component ports are schema-relative, so crossing a callable boundary
    /// must rebase them through an explicit view instead of comparing the two
    /// schemas' raw paths. Declaration identity prevents same-shaped but
    /// unrelated roots from becoming interchangeable during that rebasing.
    pub fn runtime_components_supplied_by_view(
        &self,
        source: &Self,
        view: &[LayoutEvidencePathStep],
    ) -> bool {
        self.components
            .iter()
            .filter(|component| component.is_runtime())
            .all(|component| {
                source.components.iter().any(|candidate| {
                    component.formally_supplied_by(candidate)
                        && source.projected_port(&candidate.port, view).as_ref()
                            == Some(&component.port)
                })
            })
    }

    pub fn runtime_descriptor_count(&self) -> usize {
        self.components
            .iter()
            .map(LayoutBundleComponent::runtime_descriptor_count)
            .sum()
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Update)]
pub struct CallableLayoutBundleInput<'db> {
    pub origin: CallableInputLayoutHoleOrigin,
    pub schema: LayoutBundleSchema<'db>,
}

/// The complete layout-evidence calling convention declared by one function.
///
/// This artifact is type-derived and intentionally contains no body liveness,
/// return provenance, contract field identity, or concrete assigned root.
#[derive(Debug, Clone, PartialEq, Eq, Hash, Update, Default)]
pub struct CallableLayoutBundleSignature<'db> {
    pub inputs: Vec<CallableLayoutBundleInput<'db>>,
    /// Evidence required by output components that have no compatible input
    /// component. Witnesses are ordinary input parameters in the runtime ABI.
    pub output_witnesses: LayoutBundleSchema<'db>,
    pub output: LayoutBundleSchema<'db>,
}

impl CallableLayoutBundleSignature<'_> {
    /// Whether a call must remain in the runtime body to transport layout
    /// evidence across its ABI boundary.
    ///
    /// Semantic constant folding may erase calls whose complete layout ABI is
    /// compile-time-only. Calls with any runtime input, output witness, or
    /// output component must remain explicit so the parallel evidence body can
    /// supply and receive their descriptors.
    pub fn has_runtime_evidence(&self) -> bool {
        self.inputs
            .iter()
            .any(|input| input.schema.runtime_descriptor_count() != 0)
            || self.output_witnesses.runtime_descriptor_count() != 0
            || self.output.runtime_descriptor_count() != 0
    }
}
