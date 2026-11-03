//! Projection inlining.
//!
//! A projection call opens a session: the callee's ramp runs at the call and
//! grants its yielded place to the caller, and the callee's slide runs where
//! the caller ends the session. Every projection call is inlined into its
//! caller: the ramp replaces the call, and each `End` of the session runs its
//! own copy of the slide, dispatched on the yield site the ramp took when the
//! callee yields at more than one. Callee bodies are inlined first, so the
//! sessions they open are already resolved, and recursion through
//! projections is rejected before lowering.
//!
//! A sum shape is not a value: when every yield builds a variant of one enum
//! and the caller only inspects the result, each yield sets the variant it
//! built and that variant's fields, which the caller's tests and extracts
//! read, so no enum value carries the grant.

use cranelift_entity::EntityRef;
use hir::{
    analysis::{
        semantic::{FieldIndex, SemOrigin},
        ty::ty_def::TyId,
    },
    hir_def::{BinOp, CompBinOp},
};
use rustc_hash::{FxHashMap, FxHashSet};

use crate::{
    db::MirDb,
    instance::runtime::runtime_instance_lowered_body,
    runtime::{
        Layout, LayoutId, LowerError, PlaceRoot, RBlock, RBlockId, RExpr, RLocal, RLocalId, RStmt,
        RTerminator, RuntimeBody, RuntimeCarrier, RuntimeClass, RuntimeLocalRoot,
        RuntimeProviderBindingId, ScalarClass, ScalarRepr, ScalarRole, synthetic::uint_scalar,
    },
};

pub(crate) fn inline_projections<'db>(
    db: &'db dyn MirDb,
    body: &mut RuntimeBody<'db>,
) -> Result<(), LowerError> {
    while let Some((block, stmt)) = find_stmt(body, |stmt| {
        matches!(
            stmt,
            RStmt::Assign { expr: RExpr::Call { callee, .. }, .. }
                if callee.key(db).semantic(db).is_some_and(|semantic| semantic.is_projection(db))
        )
    }) {
        let RStmt::Assign {
            dst,
            expr: RExpr::Call { callee, args },
        } = body.blocks[block.index()].stmts[stmt].clone()
        else {
            unreachable!("found a projection call")
        };
        let callee = runtime_instance_lowered_body(db, callee)?.body(db);
        inline_session(db, body, block, stmt, dst, &args, callee);
    }
    prune_unreachable_blocks(body);
    Ok(())
}

/// Replaces the call at `stmt` of `block`, which assigns the session `dst`,
/// with the callee's ramp, and each `End` of the session with its slide.
fn inline_session<'db>(
    db: &'db dyn MirDb,
    body: &mut RuntimeBody<'db>,
    block: RBlockId,
    stmt: usize,
    dst: RLocalId,
    args: &[RLocalId],
    callee: &RuntimeBody<'db>,
) {
    let splice = Splice {
        callee,
        locals: body.locals.len() as u32,
        bindings: body.provider_bindings.len() as u32,
        origin: body.stmt_origins[block.index()][stmt],
    };
    body.locals.extend(callee.locals.iter().cloned());
    body.provider_bindings
        .extend(callee.provider_bindings.iter().cloned().map(|mut binding| {
            binding.value = splice.local(binding.value);
            binding
        }));
    let yields: Vec<RBlockId> = callee
        .blocks
        .iter()
        .filter_map(|block| match block.terminator {
            RTerminator::Yield { resume, .. } => Some(resume),
            _ => None,
        })
        .collect();
    let site = (yields.len() > 1).then(|| {
        body.locals.push(RLocal {
            semantic_ty: TyId::u256(db),
            carrier: RuntimeCarrier::Value(index_class()),
            root: RuntimeLocalRoot::None,
        });
        RLocalId::new(body.locals.len() - 1)
    });
    let sum_shape = sum_shape_sites(callee).and_then(|(layout, sites)| {
        let uses = SumShapeUses::find(body, dst)?;
        Some(SumShape::new(db, body, callee, layout, sites, uses))
    });

    // The ramp runs in place of the call and continues after it.
    let after_call = split_block(body, block, stmt);
    // With several yield sites, every slide is reachable from every yield:
    // the locals a slide uses need a definition on each path, though the
    // dispatch only enters the slide of the site the ramp took.
    if site.is_some() {
        for (local, class) in slide_locals(callee, &yields) {
            push_stmt(
                body,
                block,
                splice.origin,
                RStmt::Assign {
                    dst: splice.local(local),
                    expr: RExpr::Placeholder { class },
                },
            );
        }
    }
    assert_eq!(args.len(), callee.signature.params.len());
    for (param, arg) in callee.signature.params.iter().zip(args) {
        push_stmt(
            body,
            block,
            splice.origin,
            RStmt::Assign {
                dst: splice.local(param.local),
                expr: RExpr::Use(*arg),
            },
        );
    }
    let mut site_index = 0;
    let mut exits = Vec::new();
    let ramp = splice.copy(body, |stmts, terminator| match terminator {
        RTerminator::Yield { value, .. } => {
            match (value, &sum_shape) {
                (Some(value), Some(shape)) => exits.push(shape.bind(&splice, stmts, *value)),
                (Some(value), None) => stmts.push(RStmt::Assign {
                    dst,
                    expr: RExpr::Use(*value),
                }),
                (None, _) => {}
            }
            if let Some(site) = site {
                stmts.push(RStmt::Assign {
                    dst: site,
                    expr: RExpr::ConstScalar(uint_scalar(32, site_index)),
                });
            }
            site_index += 1;
            RTerminator::Goto(after_call)
        }
        _ => RTerminator::Trap,
    });
    body.blocks[block.index()].terminator = RTerminator::Goto(ramp[0]);
    let exits: Vec<_> = callee
        .blocks
        .iter()
        .zip(&ramp)
        .filter(|(block, _)| matches!(block.terminator, RTerminator::Yield { .. }))
        .map(|(_, exit)| *exit)
        .zip(exits)
        .collect();

    // Each `End` runs its own copy of the slide, which continues after it.
    while let Some((block, stmt)) = find_stmt(
        body,
        |stmt| matches!(stmt, RStmt::End { session } if *session == dst),
    ) {
        let after_end = split_block(body, block, stmt);
        let slide = splice.copy(body, |_, terminator| match terminator {
            RTerminator::Return(_) => RTerminator::Goto(after_end),
            _ => RTerminator::Trap,
        });
        let mut resumes = yields.iter().map(|resume| slide[resume.index()]);
        let dispatch = match site {
            Some(site) => {
                let default = resumes.next_back().expect("a projection yields");
                RTerminator::SwitchScalar {
                    discr: site,
                    cases: resumes
                        .enumerate()
                        .map(|(index, resume)| (uint_scalar(32, index as u64), resume))
                        .collect(),
                    default,
                }
            }
            // A projection that never yields, as one indexing a zero-length
            // array, diverges at the call: nothing reaches the session's end.
            None => resumes.next().map_or(RTerminator::Trap, RTerminator::Goto),
        };
        body.blocks[block.index()].terminator = dispatch;
    }
    if let Some(shape) = sum_shape {
        shape.replace_uses(body, after_call, &exits);
    }
}

/// The enum value a yield of a sum shape yields.
struct SumShapeSite {
    variant: u16,
    fields: Box<[RLocalId]>,
    /// Whether the yield is the value's only use.
    only_yielded: bool,
}

/// The site each yielded callee local is.
type SumShapeSites = FxHashMap<RLocalId, SumShapeSite>;

/// When every yield of `callee` yields an enum value it builds, the enum's
/// layout and the variant and fields each yielded local builds.
fn sum_shape_sites<'db>(callee: &RuntimeBody<'db>) -> Option<(LayoutId<'db>, SumShapeSites)> {
    let mut assigned: FxHashMap<RLocalId, Vec<&RExpr<'db>>> = FxHashMap::default();
    let mut uses: FxHashMap<RLocalId, usize> = FxHashMap::default();
    for block in &callee.blocks {
        for stmt in &block.stmts {
            if let RStmt::Assign { dst, expr } = stmt {
                assigned.entry(*dst).or_default().push(expr);
                for value in expr.clone().values_mut() {
                    *uses.entry(*value).or_default() += 1;
                }
            } else {
                for local in stmt.clone().locals_mut() {
                    *uses.entry(*local).or_default() += 1;
                }
            }
        }
        for value in block.terminator.clone().values_mut() {
            *uses.entry(*value).or_default() += 1;
        }
    }
    let mut layout = None;
    let mut sites = FxHashMap::default();
    for block in &callee.blocks {
        let RTerminator::Yield { value, .. } = &block.terminator else {
            continue;
        };
        let [
            RExpr::EnumMake {
                layout: built,
                variant,
                fields,
            },
        ] = assigned.get(&(*value)?)?.as_slice()
        else {
            return None;
        };
        if *layout.get_or_insert(*built) != *built {
            return None;
        }
        sites.insert(
            (*value)?,
            SumShapeSite {
                variant: variant.index,
                fields: fields.clone(),
                only_yielded: uses.get(&(*value)?) == Some(&1),
            },
        );
    }
    Some((layout?, sites))
}

/// The uses of a projection call's result in its caller, when the caller
/// only copies it, tests its variant and extracts its fields.
struct SumShapeUses {
    /// The result and the locals copying it.
    copies: FxHashSet<RLocalId>,
    /// The tags taken of them, which only `MatchEnumTag` reads.
    tags: FxHashSet<RLocalId>,
}

impl SumShapeUses {
    fn find(body: &RuntimeBody<'_>, result: RLocalId) -> Option<Self> {
        let stmts = || body.blocks.iter().flat_map(|block| &block.stmts);
        let mut copies = FxHashSet::from_iter([result]);
        loop {
            let found = copies.len();
            for stmt in stmts() {
                if let RStmt::Assign {
                    dst,
                    expr: RExpr::Use(src),
                } = stmt
                    && copies.contains(src)
                {
                    copies.insert(*dst);
                }
            }
            if copies.len() == found {
                break;
            }
        }
        let tags: FxHashSet<_> = stmts()
            .filter_map(|stmt| match stmt {
                RStmt::Assign {
                    dst,
                    expr: RExpr::EnumTagOfValue { value },
                } if copies.contains(value) => Some(*dst),
                _ => None,
            })
            .collect();
        let mentioned = |locals: Vec<&mut RLocalId>| {
            locals
                .into_iter()
                .any(|local| copies.contains(local) || tags.contains(local))
        };
        for stmt in stmts() {
            let replaced = match stmt {
                RStmt::Assign { dst, expr } => match expr {
                    RExpr::Call { .. } => *dst == result,
                    RExpr::Use(src) => copies.contains(src),
                    RExpr::EnumTagOfValue { value } => copies.contains(value),
                    RExpr::EnumIsVariant { value, .. } | RExpr::EnumExtract { value, .. } => {
                        copies.contains(value) && !copies.contains(dst) && !tags.contains(dst)
                    }
                    _ => false,
                },
                RStmt::EnumAssertVariant { value, .. } => copies.contains(value),
                RStmt::End { session } => *session == result,
                _ => false,
            };
            if !replaced && mentioned(stmt.clone().locals_mut()) {
                return None;
            }
        }
        for block in &body.blocks {
            let matched = matches!(
                &block.terminator,
                RTerminator::MatchEnumTag { tag, .. } if tags.contains(tag)
            );
            if !matched && mentioned(block.terminator.clone().values_mut()) {
                return None;
            }
        }
        Some(Self { copies, tags })
    }
}

/// A sum shape a projection call yields, scalar-replaced in its caller.
struct SumShape<'db> {
    sites: SumShapeSites,
    /// The variant the ramp built.
    variant: RLocalId,
    /// The locals holding each variant's fields, with their classes.
    fields: Vec<Vec<(RLocalId, RuntimeClass<'db>)>>,
    uses: SumShapeUses,
}

impl<'db> SumShape<'db> {
    fn new(
        db: &'db dyn MirDb,
        body: &mut RuntimeBody<'db>,
        callee: &RuntimeBody<'db>,
        layout: LayoutId<'db>,
        sites: SumShapeSites,
        uses: SumShapeUses,
    ) -> Self {
        let Layout::Enum(data) = layout.data(db) else {
            unreachable!("a sum shape is an enum")
        };
        let mut push_local = |semantic_ty, class: RuntimeClass<'db>| {
            body.locals.push(RLocal {
                semantic_ty,
                carrier: RuntimeCarrier::Value(class),
                root: RuntimeLocalRoot::None,
            });
            RLocalId::new(body.locals.len() - 1)
        };
        let variant = push_local(TyId::u256(db), index_class());
        let fields = data
            .variants
            .iter()
            .enumerate()
            .map(|(idx, layout)| {
                let built = sites.values().find(|site| usize::from(site.variant) == idx);
                layout
                    .fields
                    .iter()
                    .enumerate()
                    .map(|(field, class)| {
                        let semantic_ty = built.map_or(TyId::u256(db), |site| {
                            callee.locals[site.fields[field].index()].semantic_ty
                        });
                        (push_local(semantic_ty, class.clone()), class.clone())
                    })
                    .collect()
            })
            .collect();
        Self {
            sites,
            variant,
            fields,
            uses,
        }
    }

    /// Sets, in place of the yield of `value`, the variant it built and its
    /// fields, and returns the variant. The enum value goes when nothing else
    /// reads it.
    fn bind(&self, splice: &Splice<'_, 'db>, stmts: &mut Vec<RStmt<'db>>, value: RLocalId) -> u16 {
        let site = &self.sites[&RLocalId::from_u32(value.as_u32() - splice.locals)];
        let (variant, built) = (site.variant, &site.fields);
        if site.only_yielded {
            stmts.retain(|stmt| {
                !matches!(stmt, RStmt::Assign { dst, expr: RExpr::EnumMake { .. } } if *dst == value)
            });
        }
        stmts.push(RStmt::Assign {
            dst: self.variant,
            expr: RExpr::ConstScalar(uint_scalar(32, u64::from(variant))),
        });
        for (field, (local, _)) in self.fields[usize::from(variant)].iter().enumerate() {
            stmts.push(RStmt::Assign {
                dst: *local,
                expr: RExpr::Use(splice.local(built[field])),
            });
        }
        variant
    }

    /// Rewrites the caller's uses of the call's result to read the variant
    /// and the fields the yields set, and sends each yield, at `exits` with
    /// the variant it built, to that variant's case of the caller's match
    /// when nothing runs between the call and the match. Each yield defines
    /// the other variants' fields too: a read under a test of the variant,
    /// such as a drop's where the caller's match joins, need not follow the
    /// yield that set them.
    fn replace_uses(
        self,
        body: &mut RuntimeBody<'db>,
        after_call: RBlockId,
        exits: &[(RBlockId, u16)],
    ) {
        let SumShapeUses { copies, tags } = &self.uses;
        let mut constants = Vec::new();
        // The variant each rewritten test compares against, by its result.
        let mut tests = FxHashMap::default();
        let first_constant = body.locals.len();
        for (block, origins) in body.blocks.iter_mut().zip(&mut body.stmt_origins) {
            let mut stmts = Vec::with_capacity(block.stmts.len());
            let mut stmt_origins = Vec::with_capacity(origins.len());
            for (stmt, origin) in block.stmts.drain(..).zip(origins.drain(..)) {
                let replaced = match stmt {
                    RStmt::Assign {
                        expr: RExpr::Use(src),
                        ..
                    } if copies.contains(&src) => vec![],
                    RStmt::EnumAssertVariant { value, .. } if copies.contains(&value) => vec![],
                    RStmt::Assign {
                        dst,
                        expr: RExpr::EnumTagOfValue { value },
                    } if copies.contains(&value) => vec![RStmt::Assign {
                        dst,
                        expr: RExpr::Use(self.variant),
                    }],
                    RStmt::Assign {
                        dst,
                        expr:
                            RExpr::EnumExtract {
                                value,
                                variant,
                                field: FieldIndex(field),
                            },
                    } if copies.contains(&value) => vec![RStmt::Assign {
                        dst,
                        expr: RExpr::Use(
                            self.fields[usize::from(variant.index)][usize::from(field)].0,
                        ),
                    }],
                    RStmt::Assign {
                        dst,
                        expr: RExpr::EnumIsVariant { value, variant },
                    } if copies.contains(&value) => {
                        tests.insert(dst, variant.index);
                        let constant = RLocalId::new(first_constant + constants.len());
                        constants.push(RLocal {
                            semantic_ty: body.locals[self.variant.index()].semantic_ty,
                            carrier: RuntimeCarrier::Value(index_class()),
                            root: RuntimeLocalRoot::None,
                        });
                        vec![
                            RStmt::Assign {
                                dst: constant,
                                expr: RExpr::ConstScalar(uint_scalar(32, u64::from(variant.index))),
                            },
                            RStmt::Assign {
                                dst,
                                expr: RExpr::Binary {
                                    op: BinOp::Comp(CompBinOp::Eq),
                                    lhs: self.variant,
                                    rhs: constant,
                                },
                            },
                        ]
                    }
                    stmt => vec![stmt],
                };
                stmt_origins.extend(std::iter::repeat_n(origin, replaced.len()));
                stmts.extend(replaced);
            }
            block.stmts = stmts;
            *origins = stmt_origins;
            if let RTerminator::MatchEnumTag {
                tag,
                cases,
                default,
                ..
            } = &block.terminator
                && tags.contains(tag)
            {
                let mut cases: Vec<_> = cases
                    .iter()
                    .map(|(variant, block)| (uint_scalar(32, u64::from(variant.index)), *block))
                    .collect();
                let default = match default {
                    Some(default) => *default,
                    None => cases.pop().expect("a match has a case").1,
                };
                block.terminator = RTerminator::SwitchScalar {
                    discr: *tag,
                    cases: cases.into(),
                    default,
                };
            }
        }
        body.locals.extend(constants);
        let constants = first_constant..body.locals.len();
        for tag in tags {
            body.locals[tag.index()].carrier = RuntimeCarrier::Value(index_class());
        }
        for &(exit, variant) in exits {
            if let Some(case) = self.dispatched_case(body, after_call, variant, &tests, &constants)
            {
                body.blocks[exit.index()].terminator = RTerminator::Goto(case);
            }
            for (idx, fields) in self.fields.iter().enumerate() {
                if idx == usize::from(variant) {
                    continue;
                }
                for (local, class) in fields {
                    push_stmt(
                        body,
                        exit,
                        body.terminator_origins[exit.index()],
                        RStmt::Assign {
                            dst: *local,
                            expr: RExpr::Placeholder {
                                class: class.clone(),
                            },
                        },
                    );
                }
            }
        }
    }

    /// The block the caller's match on the variant enters for `variant`,
    /// when the blocks from `block` to the match run nothing but read the
    /// variant: copies of it, and the `tests` that compare it with the
    /// variants in `constants`.
    fn dispatched_case(
        &self,
        body: &RuntimeBody<'db>,
        mut block: RBlockId,
        variant: u16,
        tests: &FxHashMap<RLocalId, u16>,
        constants: &std::ops::Range<usize>,
    ) -> Option<RBlockId> {
        let mut visited = FxHashSet::default();
        while visited.insert(block) {
            let data = &body.blocks[block.index()];
            if !data.stmts.iter().all(|stmt| match stmt {
                RStmt::Assign {
                    dst,
                    expr: RExpr::Use(src),
                } => self.uses.tags.contains(dst) && *src == self.variant,
                RStmt::Assign {
                    dst,
                    expr: RExpr::ConstScalar(_),
                } => constants.contains(&dst.index()),
                RStmt::Assign { dst, .. } => tests.contains_key(dst),
                _ => false,
            }) {
                return None;
            }
            match &data.terminator {
                RTerminator::Goto(next) => block = *next,
                RTerminator::Branch {
                    cond,
                    then_bb,
                    else_bb,
                } if tests.contains_key(cond) => {
                    return Some(if tests[cond] == variant {
                        *then_bb
                    } else {
                        *else_bb
                    });
                }
                RTerminator::SwitchScalar {
                    discr,
                    cases,
                    default,
                } if self.uses.tags.contains(discr) => {
                    let index = uint_scalar(32, u64::from(variant));
                    return Some(
                        cases
                            .iter()
                            .find(|(case, _)| *case == index)
                            .map_or(*default, |(_, block)| *block),
                    );
                }
                _ => return None,
            }
        }
        None
    }
}

/// The class of a yield site's or a variant's index.
fn index_class<'db>() -> RuntimeClass<'db> {
    RuntimeClass::Scalar(ScalarClass {
        repr: ScalarRepr::Int {
            bits: 32,
            signed: false,
        },
        role: ScalarRole::Plain,
    })
}

/// The SSA locals of `callee` its slides mention, with their classes.
fn slide_locals<'db>(
    callee: &RuntimeBody<'db>,
    resumes: &[RBlockId],
) -> Vec<(RLocalId, RuntimeClass<'db>)> {
    let mut seen = vec![false; callee.blocks.len()];
    let mut stack = resumes.to_vec();
    let mut locals = std::collections::BTreeSet::new();
    while let Some(block) = stack.pop() {
        if std::mem::replace(&mut seen[block.index()], true) {
            continue;
        }
        let mut data = callee.blocks[block.index()].clone();
        for stmt in &mut data.stmts {
            locals.extend(stmt.locals_mut().into_iter().map(|local| *local));
        }
        locals.extend(data.terminator.values_mut().into_iter().map(|value| *value));
        stack.extend(data.terminator.successors());
    }
    locals
        .into_iter()
        .filter_map(|local| {
            let data = &callee.locals[local.index()];
            match (&data.carrier, &data.root) {
                (RuntimeCarrier::Value(class), root)
                    if !matches!(root, RuntimeLocalRoot::Slot(_)) =>
                {
                    Some((local, class.clone()))
                }
                _ => None,
            }
        })
        .collect()
}

/// A callee body being copied into a caller.
struct Splice<'a, 'db> {
    callee: &'a RuntimeBody<'db>,
    /// The caller local of the callee's first local.
    locals: u32,
    /// The caller provider binding of the callee's first binding.
    bindings: u32,
    /// The call site, which every copied statement is attributed to.
    origin: SemOrigin<'db>,
}

impl<'db> Splice<'_, 'db> {
    fn local(&self, local: RLocalId) -> RLocalId {
        RLocalId::from_u32(self.locals + local.as_u32())
    }

    /// Appends a copy of the callee's blocks to `body`, with `exit` choosing
    /// what each `Yield` and `Return` becomes, and returns the copies by
    /// callee block.
    fn copy(
        &self,
        body: &mut RuntimeBody<'db>,
        mut exit: impl FnMut(&mut Vec<RStmt<'db>>, &RTerminator<'db>) -> RTerminator<'db>,
    ) -> Vec<RBlockId> {
        let blocks: Vec<_> = (body.blocks.len()..)
            .take(self.callee.blocks.len())
            .map(RBlockId::new)
            .collect();
        for block in &self.callee.blocks {
            let mut stmts: Vec<_> = block
                .stmts
                .iter()
                .cloned()
                .map(|mut stmt| {
                    for local in stmt.locals_mut() {
                        *local = self.local(*local);
                    }
                    if let Some(place) = stmt.place_mut()
                        && let PlaceRoot::Provider(binding) = &mut place.root
                    {
                        *binding =
                            RuntimeProviderBindingId::from_u32(self.bindings + binding.as_u32());
                    }
                    stmt
                })
                .collect();
            let mut terminator = block.terminator.clone();
            for value in terminator.values_mut() {
                *value = self.local(*value);
            }
            let terminator = match terminator {
                RTerminator::Yield { .. } | RTerminator::Return(_) => exit(&mut stmts, &terminator),
                mut terminator => {
                    for successor in terminator.successors_mut() {
                        *successor = blocks[successor.index()];
                    }
                    terminator
                }
            };
            body.stmt_origins.push(vec![self.origin; stmts.len()]);
            body.terminator_origins.push(self.origin);
            body.blocks.push(RBlock { stmts, terminator });
        }
        blocks
    }
}

fn find_stmt(
    body: &RuntimeBody<'_>,
    mut pred: impl FnMut(&RStmt<'_>) -> bool,
) -> Option<(RBlockId, usize)> {
    body.blocks.iter().enumerate().find_map(|(block, data)| {
        let stmt = data.stmts.iter().position(&mut pred)?;
        Some((RBlockId::new(block), stmt))
    })
}

/// Removes statement `stmt` of `block`, and moves the statements after it and
/// the block's terminator into a new block, which it returns.
fn split_block(body: &mut RuntimeBody<'_>, block: RBlockId, stmt: usize) -> RBlockId {
    let index = block.index();
    let stmts = body.blocks[index].stmts.split_off(stmt + 1);
    let origins = body.stmt_origins[index].split_off(stmt + 1);
    body.blocks[index].stmts.pop();
    body.stmt_origins[index].pop();
    let terminator = std::mem::replace(&mut body.blocks[index].terminator, RTerminator::Trap);
    body.stmt_origins.push(origins);
    body.terminator_origins.push(body.terminator_origins[index]);
    body.blocks.push(RBlock { stmts, terminator });
    RBlockId::new(body.blocks.len() - 1)
}

fn push_stmt<'db>(
    body: &mut RuntimeBody<'db>,
    block: RBlockId,
    origin: SemOrigin<'db>,
    stmt: RStmt<'db>,
) {
    body.blocks[block.index()].stmts.push(stmt);
    body.stmt_origins[block.index()].push(origin);
}

/// Drops the blocks the entry cannot reach: copies of ramps and slides
/// include each other's blocks.
fn prune_unreachable_blocks(body: &mut RuntimeBody<'_>) {
    fn reachable_only<T>(items: Vec<T>, reachable: &[bool]) -> Vec<T> {
        items
            .into_iter()
            .zip(reachable)
            .filter_map(|(item, reachable)| reachable.then_some(item))
            .collect()
    }
    let mut reachable = vec![false; body.blocks.len()];
    let mut stack = vec![RBlockId::new(0)];
    while let Some(block) = stack.pop() {
        if !std::mem::replace(&mut reachable[block.index()], true) {
            stack.extend(body.blocks[block.index()].terminator.successors());
        }
    }
    let renamed: Vec<_> = reachable
        .iter()
        .scan(0, |next, reachable| {
            let block = RBlockId::new(*next);
            *next += usize::from(*reachable);
            Some(block)
        })
        .collect();
    body.blocks = reachable_only(std::mem::take(&mut body.blocks), &reachable);
    for block in &mut body.blocks {
        for successor in block.terminator.successors_mut() {
            *successor = renamed[successor.index()];
        }
    }
    body.stmt_origins = reachable_only(std::mem::take(&mut body.stmt_origins), &reachable);
    body.terminator_origins =
        reachable_only(std::mem::take(&mut body.terminator_origins), &reachable);
}
