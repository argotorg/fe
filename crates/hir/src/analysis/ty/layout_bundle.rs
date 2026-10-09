use rustc_hash::FxHashMap;
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
/// roots of the logical value reached through `EffectHandle::Target`. There
/// is no index step: array elements never carry layout roots.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Update)]
pub enum LayoutEvidencePathStep {
    Field(u16),
    Variant(u16),
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

/// The stable structural identity of one physical layout-root component.
///
/// `value_path` identifies the physical occurrence in the runtime value
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
/// Every component below `alias` is the same runtime root as the corresponding
/// component below `canonical`. This represents recursive effect-target views
/// without unrolling an infinite family of path-prefixed ABI components.
#[derive(Debug, Clone, PartialEq, Eq, Hash, Update)]
pub struct LayoutViewAlias {
    pub alias: LayoutEvidencePath,
    pub canonical: LayoutEvidencePath,
}

/// A value shape that no finite layout-evidence interface can represent.
#[derive(Debug, Clone, PartialEq, Eq, Hash, Update)]
pub enum LayoutBundleUnrepresentable {
    /// A recursive effect-handle target view changes its layout arguments.
    NonRegularViewCycle {
        canonical: LayoutEvidencePath,
        recursive: LayoutEvidencePath,
    },
    /// Every element of an array shares one element type, so the elements
    /// cannot carry distinct layout roots. Arrays of root-bearing values are
    /// rejected rather than given per-element roots.
    RootArray { array: LayoutEvidencePath },
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
    pub fn from_index(index: usize) -> Self {
        Self(u32::try_from(index).expect("layout bundle has more than u32::MAX components"))
    }

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

/// Whether a component is part of a particular runtime layout-evidence ABI.
///
/// This is intentionally independent of [`LayoutBundleComponentKey`]. Generic
/// specialization can give a declaration-level layout parameter a concrete
/// representative while the value's physical occurrence still supplies a
/// different root. Conversely, a concrete component written in the callable's
/// declaration needs no runtime transport.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Update)]
pub enum LayoutBundleComponentTransport {
    CompileTime,
    Runtime,
}

/// One physical runtime-root transport.
///
/// A component has exactly one occurrence. Distinct value paths never
/// share a descriptor merely because generic substitution makes their const
/// arguments equal: value transformations can change either path
/// independently. `port` retains physical occurrence identity,
/// `declaration` relates components through the callable's declared type, and
/// `representative` is the instantiated root key, used only for allocation
/// lookup and explicit const resolution.
#[derive(Debug, Clone, PartialEq, Eq, Hash, Update)]
pub struct LayoutBundleComponent<'db> {
    pub port: LayoutPortKey,
    pub declaration: LayoutBundleComponentDeclaration<'db>,
    pub representative: LayoutBundleComponentKey<'db>,
    pub ty: TyId<'db>,
    /// Declaration-level const parameters whose exact scalar value is
    /// supplied by this port. These identities survive specialization:
    /// instantiated value equality is not evidence provenance.
    pub supplied_const_params: Vec<TyId<'db>>,
    /// Declaration-level const parameters used to compute this port's value.
    /// A dependency is deliberately not a supplied value: `{ROOT + 1}` cannot
    /// reify `ROOT` at runtime.
    pub dependent_const_params: Vec<TyId<'db>>,
}

impl<'db> LayoutBundleComponent<'db> {
    /// Whether this component's formal root is available from `candidate`
    /// inside the same callable declaration. Exact declaration identity is
    /// required for explicit const expressions: sharing dependencies does not
    /// make `ROOT` and `ROOT + 1` the same layout value. Formal-parameter
    /// fallback exists only for structural roots re-anchored at a boundary.
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
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Update, Default)]
pub struct LayoutBundleSchema<'db> {
    pub components: Vec<LayoutBundleComponent<'db>>,
    pub view_aliases: Vec<LayoutViewAlias>,
    pub unrepresentable: Option<LayoutBundleUnrepresentable>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Update)]
pub enum LayoutBundleSchemaError {
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
    Unrepresentable(LayoutBundleUnrepresentable),
}

impl<'db> LayoutBundleSchema<'db> {
    /// Whether the value carries no layout. An unrepresentable schema is never
    /// empty: it records a value shape that cannot cross a callable boundary.
    pub fn is_empty(&self) -> bool {
        self.components.is_empty() && self.unrepresentable.is_none()
    }

    pub fn validate(&self) -> Result<(), LayoutBundleSchemaError> {
        if let Some(unrepresentable) = &self.unrepresentable {
            return Err(LayoutBundleSchemaError::Unrepresentable(
                unrepresentable.clone(),
            ));
        }
        for (idx, alias) in self.view_aliases.iter().enumerate() {
            if alias.alias.len() <= alias.canonical.len()
                || !alias.alias.starts_with(&alias.canonical)
            {
                return Err(LayoutBundleSchemaError::InvalidViewAlias { alias: idx });
            }
        }
        // Establish every rule's decreasing measure before following any rule.
        // A later malformed rule can otherwise trap an earlier target walk.
        for (idx, alias) in self.view_aliases.iter().enumerate() {
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
        }
        for (idx, alias) in self.view_aliases.iter().enumerate() {
            if self.canonicalize_view_path(&alias.canonical)? != alias.canonical {
                return Err(LayoutBundleSchemaError::InvalidViewAlias { alias: idx });
            }
        }
        let mut component_ports = FxHashMap::default();
        for (idx, component) in self.components.iter().enumerate() {
            let id = LayoutBundleComponentId::from_index(idx);
            if let Some(first) = component_ports.insert(&component.port, id) {
                return Err(LayoutBundleSchemaError::DuplicateComponent { first, second: id });
            }
            if component
                .supplied_const_params
                .iter()
                .enumerate()
                .any(|(idx, param)| component.supplied_const_params[..idx].contains(param))
            {
                return Err(LayoutBundleSchemaError::DuplicateSuppliedConstParam { component: id });
            }
            if component
                .dependent_const_params
                .iter()
                .enumerate()
                .any(|(idx, param)| component.dependent_const_params[..idx].contains(param))
            {
                return Err(LayoutBundleSchemaError::DuplicateDependentConstParam {
                    component: id,
                });
            }
            if component
                .supplied_const_params
                .iter()
                .any(|param| !component.dependent_const_params.contains(param))
            {
                return Err(LayoutBundleSchemaError::InvalidSuppliedConstParam { component: id });
            }
            if self.canonicalize_view_path(&component.port.value_path)? != component.port.value_path
            {
                return Err(LayoutBundleSchemaError::NonCanonicalComponent { component: id });
            }
        }
        Ok(())
    }

    /// Canonicalizes with at most the original path length's worth of rewrites.
    /// Each selected rule must strictly shorten the path, even for raw schemas.
    /// Full schema validation remains the publication boundary; this check does
    /// not revalidate the entire alias table on each projection.
    pub fn canonicalize_view_path(
        &self,
        path: &[LayoutEvidencePathStep],
    ) -> Result<LayoutEvidencePath, LayoutBundleSchemaError> {
        let mut path = path.to_vec();
        loop {
            let Some((idx, alias)) = self
                .view_aliases
                .iter()
                .enumerate()
                .filter(|(_, alias)| path.starts_with(&alias.alias))
                .max_by_key(|(_, alias)| alias.alias.len())
            else {
                return Ok(path);
            };
            if alias.alias.len() <= alias.canonical.len()
                || !alias.alias.starts_with(&alias.canonical)
            {
                return Err(LayoutBundleSchemaError::InvalidViewAlias { alias: idx });
            }
            path.drain(alias.canonical.len()..alias.alias.len());
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
    ) -> Result<Option<LayoutPortKey>, LayoutBundleSchemaError> {
        let prefix = self.canonicalize_view_path(prefix)?;
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
        Ok(direct
            .map(<[_]>::to_vec)
            .or(aliased)
            .map(|value_path| LayoutPortKey {
                value_path,
                root: port.root,
            }))
    }

    pub fn canonicalize_port(
        &self,
        port: &LayoutPortKey,
    ) -> Result<LayoutPortKey, LayoutBundleSchemaError> {
        Ok(LayoutPortKey {
            value_path: self.canonicalize_view_path(&port.value_path)?,
            root: port.root,
        })
    }

    pub fn indexed_components(
        &self,
    ) -> impl ExactSizeIterator<Item = (LayoutBundleComponentId, &LayoutBundleComponent<'db>)> + '_
    {
        self.components
            .iter()
            .enumerate()
            .map(|(idx, component)| (LayoutBundleComponentId::from_index(idx), component))
    }

    pub fn component(&self, id: LayoutBundleComponentId) -> Option<&LayoutBundleComponent<'db>> {
        self.components.get(id.index())
    }

    pub fn component_by_port(
        &self,
        port: &LayoutPortKey,
    ) -> Option<(LayoutBundleComponentId, &LayoutBundleComponent<'db>)> {
        self.indexed_components()
            .find(|(_, component)| component.port == *port)
    }
}

/// The context-specific transport policy for one structural bundle schema.
///
/// Entries are indexed by [`LayoutBundleComponentId`]. Keeping this mask out of
/// [`LayoutBundleSchema`] lets one immutable value shape participate in a
/// declaration ABI, a specialized ABI, and local evidence SSA without mutating
/// its structural component metadata.
#[derive(Debug, Clone, PartialEq, Eq, Hash, Update, Default)]
pub struct LayoutBundleTransport {
    pub components: Vec<LayoutBundleComponentTransport>,
}

impl LayoutBundleTransport {
    pub fn inferred(schema: &LayoutBundleSchema<'_>) -> Self {
        Self {
            components: schema
                .components
                .iter()
                .map(|component| {
                    if matches!(
                        component.representative,
                        LayoutBundleComponentKey::Static(_)
                    ) {
                        LayoutBundleComponentTransport::CompileTime
                    } else {
                        LayoutBundleComponentTransport::Runtime
                    }
                })
                .collect(),
        }
    }

    pub fn all_runtime(schema: &LayoutBundleSchema<'_>) -> Self {
        Self {
            components: vec![LayoutBundleComponentTransport::Runtime; schema.components.len()],
        }
    }

    pub fn component(&self, id: LayoutBundleComponentId) -> Option<LayoutBundleComponentTransport> {
        self.components.get(id.index()).copied()
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Update)]
pub enum LayoutBundleInterfaceError {
    Schema(LayoutBundleSchemaError),
    ComponentCount { expected: usize, actual: usize },
    InvalidCompileTimeComponent { component: LayoutBundleComponentId },
}

/// A structural bundle schema paired with the transport policy of one ABI.
#[derive(Debug, Clone, PartialEq, Eq, Hash, Update, Default)]
pub struct LayoutBundleInterface<'db> {
    pub schema: LayoutBundleSchema<'db>,
    pub transport: LayoutBundleTransport,
}

impl<'db> LayoutBundleInterface<'db> {
    pub fn inferred(schema: LayoutBundleSchema<'db>) -> Self {
        let transport = LayoutBundleTransport::inferred(&schema);
        Self { schema, transport }
    }

    pub fn all_runtime(schema: LayoutBundleSchema<'db>) -> Self {
        let transport = LayoutBundleTransport::all_runtime(&schema);
        Self { schema, transport }
    }

    pub fn validate(&self) -> Result<(), LayoutBundleInterfaceError> {
        self.schema
            .validate()
            .map_err(LayoutBundleInterfaceError::Schema)?;
        if self.schema.components.len() != self.transport.components.len() {
            return Err(LayoutBundleInterfaceError::ComponentCount {
                expected: self.schema.components.len(),
                actual: self.transport.components.len(),
            });
        }
        for (id, component, transport) in self.indexed_components_with_transport() {
            if matches!(transport, LayoutBundleComponentTransport::CompileTime)
                && !matches!(
                    component.representative,
                    LayoutBundleComponentKey::Static(_)
                )
            {
                return Err(LayoutBundleInterfaceError::InvalidCompileTimeComponent {
                    component: id,
                });
            }
        }
        Ok(())
    }

    fn indexed_components_with_transport(
        &self,
    ) -> impl ExactSizeIterator<
        Item = (
            LayoutBundleComponentId,
            &LayoutBundleComponent<'db>,
            LayoutBundleComponentTransport,
        ),
    > + '_ {
        assert_eq!(
            self.schema.components.len(),
            self.transport.components.len(),
            "layout bundle schema and transport must have the same component count"
        );
        self.schema
            .indexed_components()
            .map(|(id, component)| (id, component, self.transport.components[id.index()]))
    }

    pub fn is_runtime(&self, id: LayoutBundleComponentId) -> bool {
        matches!(
            self.transport.component(id),
            Some(LayoutBundleComponentTransport::Runtime)
        )
    }

    pub fn runtime_components(
        &self,
    ) -> impl Iterator<Item = (LayoutBundleComponentId, &LayoutBundleComponent<'db>)> + '_ {
        self.indexed_components_with_transport()
            .filter_map(|(id, component, transport)| {
                matches!(transport, LayoutBundleComponentTransport::Runtime)
                    .then_some((id, component))
            })
    }

    pub fn runtime_descriptor_count(&self) -> usize {
        self.runtime_components().count()
    }

    fn runtime_mapping(
        &self,
        source: &LayoutBundleSchema<'db>,
        view: &[LayoutEvidencePathStep],
        mut compatible: impl FnMut(&LayoutBundleComponent<'db>, &LayoutBundleComponent<'db>) -> bool,
    ) -> Result<Option<LayoutBundleViewMapping>, LayoutBundleSchemaError> {
        let mut sources = vec![None; self.schema.components.len()];
        for (target, component) in self.runtime_components() {
            let mut selected = None;
            for (id, candidate) in source.indexed_components() {
                if compatible(component, candidate)
                    && source.projected_port(&candidate.port, view)?.as_ref()
                        == Some(&component.port)
                    && selected.replace(id).is_some()
                {
                    return Ok(None);
                }
            }
            if selected.is_none() {
                return Ok(None);
            }
            sources[target.index()] = selected;
        }
        Ok(Some(LayoutBundleViewMapping { sources }))
    }

    /// Maps a type-checked value projection to a specialized callable input.
    ///
    /// The semantic type relation already proves that the value may cross the
    /// boundary. Substitution and structural-hole re-anchoring can change root
    /// declarations, so exact port and root type are the remaining transport
    /// identity at this boundary.
    pub fn runtime_call_mapping(
        &self,
        source: &LayoutBundleSchema<'db>,
        view: &[LayoutEvidencePathStep],
    ) -> Result<Option<LayoutBundleViewMapping>, LayoutBundleSchemaError> {
        self.runtime_mapping(source, view, |target, candidate| target.ty == candidate.ty)
    }

    /// Maps every runtime target component to its unique source component in
    /// one semantic view.
    ///
    /// Unlike a direct type-checked projection, selecting among semantic views
    /// must retain declaration identity so same-shaped but unrelated physical
    /// and logical roots cannot become interchangeable.
    pub fn runtime_view_mapping(
        &self,
        source: &LayoutBundleSchema<'db>,
        view: &[LayoutEvidencePathStep],
    ) -> Result<Option<LayoutBundleViewMapping>, LayoutBundleSchemaError> {
        self.runtime_mapping(source, view, |target, candidate| {
            target.ty == candidate.ty && target.formally_derivable_from(candidate)
        })
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Update, Default)]
pub struct LayoutBundleViewMapping {
    sources: Vec<Option<LayoutBundleComponentId>>,
}

impl LayoutBundleViewMapping {
    pub fn source(&self, target: LayoutBundleComponentId) -> Option<LayoutBundleComponentId> {
        self.sources.get(target.index()).copied().flatten()
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Update)]
pub struct CallableLayoutBundleInput<'db> {
    pub origin: CallableInputLayoutHoleOrigin,
    pub interface: LayoutBundleInterface<'db>,
}

pub struct CallableLayoutBundleParam<'a, 'db> {
    pub source: CallableLayoutParamPort,
    pub component_id: LayoutBundleComponentId,
    pub component: &'a LayoutBundleComponent<'db>,
}

pub struct CallableLayoutBundleResult<'a, 'db> {
    pub component_id: LayoutBundleComponentId,
    pub component: &'a LayoutBundleComponent<'db>,
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
    pub output_witnesses: LayoutBundleInterface<'db>,
    pub output: LayoutBundleInterface<'db>,
}

impl<'db> CallableLayoutBundleSignature<'db> {
    pub fn input(
        &self,
        origin: CallableInputLayoutHoleOrigin,
    ) -> Option<&CallableLayoutBundleInput<'db>> {
        self.inputs.iter().find(|input| input.origin == origin)
    }

    /// Runtime evidence parameters in their canonical ABI order.
    pub fn runtime_params(&self) -> impl Iterator<Item = CallableLayoutBundleParam<'_, 'db>> + '_ {
        self.inputs
            .iter()
            .flat_map(|input| {
                input
                    .interface
                    .runtime_components()
                    .map(move |(component_id, component)| CallableLayoutBundleParam {
                        source: CallableLayoutParamPort::Input(CallableLayoutPort {
                            origin: input.origin,
                            component: component.port.clone(),
                        }),
                        component_id,
                        component,
                    })
            })
            .chain(
                self.output_witnesses
                    .runtime_components()
                    .map(|(component_id, component)| CallableLayoutBundleParam {
                        source: CallableLayoutParamPort::OutputWitness(component.port.clone()),
                        component_id,
                        component,
                    }),
            )
    }

    /// Runtime evidence results in their canonical ABI order.
    pub fn runtime_results(
        &self,
    ) -> impl Iterator<Item = CallableLayoutBundleResult<'_, 'db>> + '_ {
        self.output
            .runtime_components()
            .map(|(component_id, component)| CallableLayoutBundleResult {
                component_id,
                component,
            })
    }

    /// Whether a call must remain in the runtime body to transport layout
    /// evidence across its ABI boundary.
    ///
    /// Semantic constant folding may erase calls whose complete layout ABI is
    /// compile-time-only. Calls with any runtime input, output witness, or
    /// output component must remain explicit so the parallel evidence body can
    /// supply and receive their descriptors.
    pub fn has_runtime_evidence(&self) -> bool {
        self.runtime_params().next().is_some() || self.runtime_results().next().is_some()
    }
}
