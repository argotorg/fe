//! Successful halts while lane copies are held.
//!
//! A `mut` access of a lane works on a memory copy, which the end of the
//! access stores back (`hold_lane`). A successful halt (RETURN, STOP,
//! SELFDESTRUCT) ends the execution with storage as it stands, so every copy
//! held anywhere on the call stack must be stored back first. Lowering marks
//! where each copy is held (`RStmt::HoldLane`, `RStmt::ReleaseLane`); once
//! projections are inlined, `unwind_halts` resolves the marks:
//!
//! - a halt first stores back the copies its body holds there;
//! - a call that may halt, made while the body holds copies, goes to the
//!   callee's unwinding variant (`RuntimeInstanceKey::unwinds`) with a halt
//!   record. The variant returns from a halt instead, having recorded it,
//!   and the caller stores back its copies and then halts as recorded.
//!
//! An unwinding variant records every halt it reaches, its own or a
//! callee's, and returns, so each frame on the way stores back its copies. A
//! body that holds no copy where it may halt is left as it is.

use std::collections::BTreeSet;

use cranelift_entity::EntityRef;
use hir::{
    analysis::{
        semantic::{FieldIndex, SemOrigin},
        ty::{AddressSpace, ty_def::TyId},
    },
    hir_def::{BinOp, CompBinOp},
};
use rustc_hash::FxHashMap;
use salsa::SalsaValue;

use crate::{
    db::MirDb,
    instance::{
        RuntimeInstance, RuntimeInstanceKey,
        runtime::{get_or_build_runtime_instance, runtime_body_with_lane_holds},
    },
    runtime::{
        ConstScalar, LayoutId, LayoutKey, PlaceElem, PlaceRoot, RBlock, RBlockId, RExpr, RLocal,
        RLocalId, RStmt, RTerminator, RefKind, RefView, RuntimeBody, RuntimeBuiltin,
        RuntimeCarrier, RuntimeClass, RuntimeLocalRoot, RuntimeParam, RuntimePlace, ScalarClass,
        ScalarRepr, ScalarRole, StructLayout, synthetic::uint_scalar,
    },
};

use super::{abi::runtime_declaration_signature, type_info::pointer_class};

/// A successful halt, by the code its halt record holds.
#[derive(Clone, Copy, PartialEq, Eq)]
enum HaltKind {
    Return = 1,
    Stop = 2,
    SelfDestruct = 3,
}

impl HaltKind {
    const ALL: [Self; 3] = [Self::Return, Self::Stop, Self::SelfDestruct];

    fn of(terminator: &RTerminator<'_>) -> Option<Self> {
        match terminator {
            RTerminator::ReturnData { .. } => Some(Self::Return),
            RTerminator::Stop => Some(Self::Stop),
            RTerminator::SelfDestruct { .. } => Some(Self::SelfDestruct),
            _ => None,
        }
    }
}

/// The kinds of successful halt a call can reach.
#[derive(Clone, Copy, Debug, Default, PartialEq, Eq, Hash, SalsaValue)]
pub struct HaltKinds(u8);

impl HaltKinds {
    fn with(self, kind: HaltKind) -> Self {
        Self(self.0 | 1 << kind as u8)
    }

    fn union(self, other: Self) -> Self {
        Self(self.0 | other.0)
    }

    fn is_empty(self) -> bool {
        self.0 == 0
    }

    fn iter(self) -> impl Iterator<Item = HaltKind> {
        HaltKind::ALL
            .into_iter()
            .filter(move |kind| self.0 & 1 << *kind as u8 != 0)
    }
}

/// The successful halts a call to `instance` can reach, through its own
/// terminators and the calls it makes.
#[salsa::tracked(returns(copy), cycle_initial = halt_kinds_cycle_initial)]
pub(crate) fn runtime_halt_kinds<'db>(
    db: &'db dyn MirDb,
    instance: RuntimeInstance<'db>,
) -> HaltKinds {
    if instance.is_external_declaration(db) {
        return HaltKinds::default();
    }
    let Ok(body) = runtime_body_with_lane_holds(db, instance) else {
        return HaltKinds::default();
    };
    body.blocks
        .iter()
        .fold(HaltKinds::default(), |kinds, block| {
            let calls = block.stmts.iter().filter_map(|stmt| match stmt {
                RStmt::Assign {
                    expr: RExpr::Call { callee, .. },
                    ..
                } => Some(*callee),
                _ => None,
            });
            let kinds = match &block.terminator {
                RTerminator::TerminalCall { callee, .. } => {
                    kinds.union(runtime_halt_kinds(db, *callee))
                }
                terminator => HaltKind::of(terminator).map_or(kinds, |kind| kinds.with(kind)),
            };
            calls.fold(kinds, |kinds, callee| {
                kinds.union(runtime_halt_kinds(db, callee))
            })
        })
}

fn halt_kinds_cycle_initial<'db>(
    _: &'db dyn MirDb,
    _: salsa::Id,
    _: RuntimeInstance<'db>,
) -> HaltKinds {
    HaltKinds::default()
}

/// The halt record an unwinding variant takes last: the halt's kind, its
/// return data's address and its length or beneficiary, in that order.
fn halt_record_layout<'db>(db: &'db dyn MirDb) -> LayoutId<'db> {
    LayoutId::new(
        db,
        LayoutKey::Struct(StructLayout {
            fields: [
                word_class(),
                RuntimeClass::Scalar(pointer_class(db, AddressSpace::Memory)),
                word_class(),
            ]
            .into(),
            pinned: false,
            lane_bytes: None,
        }),
    )
}

/// The class of the reference to a halt record an unwinding variant takes.
pub(crate) fn halt_record_class<'db>(db: &'db dyn MirDb) -> RuntimeClass<'db> {
    RuntimeClass::Ref {
        pointee: Box::new(RuntimeClass::AggregateValue {
            layout: halt_record_layout(db),
        }),
        kind: RefKind::Object,
        view: RefView::Whole,
    }
}

const KIND: u16 = 0;
const DATA: u16 = 1;
const WORD: u16 = 2;

fn word_class<'db>() -> RuntimeClass<'db> {
    RuntimeClass::Scalar(ScalarClass {
        repr: ScalarRepr::Int {
            bits: 256,
            signed: false,
        },
        role: ScalarRole::Plain,
    })
}

fn bool_class<'db>() -> RuntimeClass<'db> {
    RuntimeClass::Scalar(ScalarClass {
        repr: ScalarRepr::Bool,
        role: ScalarRole::Plain,
    })
}

/// The copies held at a point: on every path to it, and on some.
#[derive(Clone, Debug, Default, PartialEq, Eq)]
struct Held {
    must: BTreeSet<RLocalId>,
    may: BTreeSet<RLocalId>,
}

impl Held {
    fn apply(&mut self, stmt: &RStmt<'_>) {
        match stmt {
            RStmt::HoldLane { copy, .. } => {
                self.must.insert(*copy);
                self.may.insert(*copy);
            }
            RStmt::ReleaseLane { copy } => {
                self.must.remove(copy);
                self.may.remove(copy);
            }
            _ => {}
        }
    }

    fn meet(&self, other: &Self) -> Self {
        Self {
            must: self.must.intersection(&other.must).copied().collect(),
            may: self.may.union(&other.may).copied().collect(),
        }
    }

    /// The copies held on some paths only, which a flush tests a flag for.
    fn uncertain(&self) -> impl Iterator<Item = RLocalId> + '_ {
        self.may.difference(&self.must).copied()
    }
}

/// The copies held where each block starts, `None` where the entry does
/// not reach.
fn held_at_entry(body: &RuntimeBody<'_>) -> Vec<Option<Held>> {
    let mut entry = vec![None; body.blocks.len()];
    entry[0] = Some(Held::default());
    let mut pending = vec![RBlockId::new(0)];
    while let Some(block) = pending.pop() {
        let mut held = entry[block.index()]
            .clone()
            .expect("a pending block is reached");
        for stmt in &body.blocks[block.index()].stmts {
            held.apply(stmt);
        }
        for successor in body.blocks[block.index()].terminator.successors() {
            let joined = match &entry[successor.index()] {
                Some(old) => old.meet(&held),
                None => held.clone(),
            };
            if entry[successor.index()].as_ref() != Some(&joined) {
                entry[successor.index()] = Some(joined);
                pending.push(successor);
            }
        }
    }
    entry
}

/// Resolves the lane holds `body` marks (see the module docs).
pub(crate) fn unwind_halts<'db>(db: &'db dyn MirDb, body: &mut RuntimeBody<'db>) {
    let unwinds = body.key.unwinds(db);
    let flushes: FxHashMap<RLocalId, Box<[RStmt<'db>]>> = body
        .blocks
        .iter()
        .flat_map(|block| &block.stmts)
        .filter_map(|stmt| match stmt {
            RStmt::HoldLane { copy, flush } => Some((*copy, flush.clone())),
            _ => None,
        })
        .collect();
    if flushes.is_empty() && !unwinds {
        return;
    }
    let entry = held_at_entry(body);
    let mut unwinder = Unwinder {
        db,
        body,
        unwinds,
        flushes,
        flags: FxHashMap::default(),
        record: None,
    };
    let flag_inits = unwinder.flag_uncertain_holds(&entry);
    if unwinds {
        let class = halt_record_class(db);
        let param = unwinder.local(
            RuntimeCarrier::Value(class.clone()),
            RuntimeLocalRoot::Ref(class.clone()),
        );
        unwinder.body.signature.params.push(RuntimeParam {
            local: param,
            class,
        });
        unwinder.record = Some(PlaceRoot::Ref(param));
    }
    for (block, held) in entry.into_iter().enumerate() {
        unwinder.rewrite_block(RBlockId::new(block), held);
    }
    if !flag_inits.is_empty() {
        unwinder.prepend_entry(flag_inits);
    }
}

struct Unwinder<'a, 'db> {
    db: &'db dyn MirDb,
    body: &'a mut RuntimeBody<'db>,
    unwinds: bool,
    /// The statements storing back each held copy.
    flushes: FxHashMap<RLocalId, Box<[RStmt<'db>]>>,
    /// The flag of each copy held on some paths only to a point that may
    /// halt: whether it is held.
    flags: FxHashMap<RLocalId, RLocalId>,
    /// The halt record: the one an unwinding variant takes, or the body's
    /// own, made for its first unwinding call.
    record: Option<PlaceRoot<'db>>,
}

impl<'db> Unwinder<'_, 'db> {
    fn local(&mut self, carrier: RuntimeCarrier<'db>, root: RuntimeLocalRoot<'db>) -> RLocalId {
        let semantic_ty = match &carrier {
            RuntimeCarrier::Erased => TyId::unit(self.db),
            RuntimeCarrier::Value(class) if *class == bool_class() => TyId::bool(self.db),
            RuntimeCarrier::Value(_) => TyId::u256(self.db),
        };
        self.body.locals.push(RLocal {
            semantic_ty,
            carrier,
            root,
        });
        RLocalId::new(self.body.locals.len() - 1)
    }

    fn value(&mut self, class: RuntimeClass<'db>) -> RLocalId {
        self.local(RuntimeCarrier::Value(class), RuntimeLocalRoot::None)
    }

    fn push(&mut self, block: RBlockId, stmt: RStmt<'db>, origin: SemOrigin<'db>) {
        self.body.blocks[block.index()].stmts.push(stmt);
        self.body.stmt_origins[block.index()].push(origin);
    }

    fn assign(
        &mut self,
        block: RBlockId,
        class: RuntimeClass<'db>,
        expr: RExpr<'db>,
        origin: SemOrigin<'db>,
    ) -> RLocalId {
        let dst = self.value(class);
        self.push(block, RStmt::Assign { dst, expr }, origin);
        dst
    }

    fn new_block(&mut self, origin: SemOrigin<'db>) -> RBlockId {
        self.body.blocks.push(RBlock {
            stmts: Vec::new(),
            terminator: RTerminator::Trap,
        });
        self.body.stmt_origins.push(Vec::new());
        self.body.terminator_origins.push(origin);
        RBlockId::new(self.body.blocks.len() - 1)
    }

    fn terminate(&mut self, block: RBlockId, terminator: RTerminator<'db>, origin: SemOrigin<'db>) {
        self.body.blocks[block.index()].terminator = terminator;
        self.body.terminator_origins[block.index()] = origin;
    }

    /// The halts a call to `callee` can reach, when the body must take
    /// them from its unwinding variant: it unwinds, or holds copies.
    fn unwound(&self, callee: RuntimeInstance<'db>, held: &Held) -> Option<HaltKinds> {
        let kinds = runtime_halt_kinds(self.db, callee);
        (!kinds.is_empty() && (self.unwinds || !held.may.is_empty())).then_some(kinds)
    }

    /// Whether a halting terminator stores back copies or unwinds.
    fn handles_halt(&self, terminator: &RTerminator<'db>, held: &Held) -> bool {
        HaltKind::of(terminator).is_some() && (self.unwinds || !held.may.is_empty())
    }

    /// Gives a flag to each copy held on some paths only to a point that
    /// may halt, set while the copy is held. Returns what the entry runs
    /// first: the flags start false, and the locals the copies' flushes read
    /// get a placeholder, so they are defined on every path.
    fn flag_uncertain_holds(&mut self, entry: &[Option<Held>]) -> Vec<RStmt<'db>> {
        let mut uncertain = BTreeSet::new();
        for (block, held) in entry.iter().enumerate() {
            let Some(mut held) = held.clone() else {
                continue;
            };
            let data = &self.body.blocks[block];
            for stmt in &data.stmts {
                if let RStmt::Assign {
                    expr: RExpr::Call { callee, .. },
                    ..
                } = stmt
                    && self.unwound(*callee, &held).is_some()
                {
                    uncertain.extend(held.uncertain());
                }
                held.apply(stmt);
            }
            let halts = match &data.terminator {
                RTerminator::TerminalCall { callee, .. } => self.unwound(*callee, &held).is_some(),
                terminator => self.handles_halt(terminator, &held),
            };
            if halts {
                uncertain.extend(held.uncertain());
            }
        }
        if uncertain.is_empty() {
            return Vec::new();
        }
        let params: BTreeSet<_> = self
            .body
            .signature
            .params
            .iter()
            .map(|param| param.local)
            .collect();
        let mut inits = Vec::new();
        let mut defined = BTreeSet::new();
        for copy in uncertain {
            let flag = self.value(bool_class());
            self.flags.insert(copy, flag);
            inits.push(RStmt::Assign {
                dst: flag,
                expr: RExpr::ConstScalar(ConstScalar::Bool(false)),
            });
            for stmt in self.flushes[&copy].clone().iter_mut() {
                for local in stmt.locals_mut() {
                    let data = &self.body.locals[local.index()];
                    if let (
                        RuntimeCarrier::Value(class),
                        RuntimeLocalRoot::None
                        | RuntimeLocalRoot::Ref(_)
                        | RuntimeLocalRoot::Ptr { .. },
                    ) = (&data.carrier, &data.root)
                        && !params.contains(local)
                        && defined.insert(*local)
                    {
                        inits.push(RStmt::Assign {
                            dst: *local,
                            expr: RExpr::Placeholder {
                                class: class.clone(),
                            },
                        });
                    }
                }
            }
        }
        inits
    }

    /// Runs `stmts` once, before the entry block, which a loop may enter
    /// again.
    fn prepend_entry(&mut self, stmts: Vec<RStmt<'db>>) {
        let entry = RBlockId::new(0);
        let origin = self.body.terminator_origins[0];
        let body = self.new_block(origin);
        for data in &mut self.body.blocks {
            for successor in data.terminator.successors_mut() {
                if *successor == entry {
                    *successor = body;
                }
            }
        }
        self.body.blocks.swap(entry.index(), body.index());
        self.body.stmt_origins.swap(entry.index(), body.index());
        self.body
            .terminator_origins
            .swap(entry.index(), body.index());
        for stmt in stmts {
            self.push(entry, stmt, origin);
        }
        self.terminate(entry, RTerminator::Goto(body), origin);
    }

    fn record_root(&mut self) -> PlaceRoot<'db> {
        if let Some(root) = &self.record {
            return root.clone();
        }
        let class = RuntimeClass::AggregateValue {
            layout: halt_record_layout(self.db),
        };
        let root = PlaceRoot::Slot(self.local(
            RuntimeCarrier::Value(class.clone()),
            RuntimeLocalRoot::Slot(class),
        ));
        self.record = Some(root.clone());
        root
    }

    fn record_place(&mut self, field: u16) -> RuntimePlace<'db> {
        RuntimePlace {
            root: self.record_root(),
            path: Box::new([PlaceElem::Field(FieldIndex(field))]),
        }
    }

    fn word_const(&mut self, block: RBlockId, value: u64, origin: SemOrigin<'db>) -> RLocalId {
        self.assign(
            block,
            word_class(),
            RExpr::ConstScalar(uint_scalar(256, value)),
            origin,
        )
    }

    fn store_record(&mut self, block: RBlockId, field: u16, src: RLocalId, origin: SemOrigin<'db>) {
        let dst = self.record_place(field);
        self.push(block, RStmt::Store { dst, src }, origin);
    }

    fn load_record(
        &mut self,
        block: RBlockId,
        field: u16,
        class: RuntimeClass<'db>,
        origin: SemOrigin<'db>,
    ) -> RLocalId {
        let place = self.record_place(field);
        self.assign(block, class, RExpr::Load { place }, origin)
    }

    /// The arguments of a call to `callee`'s unwinding variant, and the
    /// variant: `args` and the halt record, which a body that does not
    /// unwind clears first.
    fn unwinding_call(
        &mut self,
        block: RBlockId,
        callee: RuntimeInstance<'db>,
        args: &[RLocalId],
        origin: SemOrigin<'db>,
    ) -> (RuntimeInstance<'db>, Box<[RLocalId]>) {
        let record = match self.record_root() {
            PlaceRoot::Ref(record) => record,
            root => {
                let zero = self.word_const(block, 0, origin);
                self.store_record(block, KIND, zero, origin);
                self.assign(
                    block,
                    halt_record_class(self.db),
                    RExpr::AddrOf {
                        place: RuntimePlace {
                            root,
                            path: Box::default(),
                        },
                    },
                    origin,
                )
            }
        };
        let key = callee.key(self.db);
        let variant = get_or_build_runtime_instance(
            self.db,
            RuntimeInstanceKey::new(
                self.db,
                key.source(self.db),
                key.params(self.db).clone(),
                true,
            ),
        );
        (variant, args.iter().copied().chain([record]).collect())
    }

    /// Stores back the copies `held` in `block`, and returns the block that
    /// goes on: a copy held on some paths only is stored back when its flag
    /// is set.
    fn flush(&mut self, mut block: RBlockId, held: &Held, origin: SemOrigin<'db>) -> RBlockId {
        for copy in &held.must {
            for stmt in self.flushes[copy].clone() {
                self.push(block, stmt, origin);
            }
        }
        for copy in held.uncertain().collect::<Vec<_>>() {
            let (store, next) = (self.new_block(origin), self.new_block(origin));
            let cond = self.flags[&copy];
            self.terminate(
                block,
                RTerminator::Branch {
                    cond,
                    then_bb: store,
                    else_bb: next,
                },
                origin,
            );
            for stmt in self.flushes[&copy].clone() {
                self.push(store, stmt, origin);
            }
            self.terminate(store, RTerminator::Goto(next), origin);
            block = next;
        }
        block
    }

    /// Ends `block` after a callee reached one of `kinds` and recorded it:
    /// an unwinding body returns, and any other halts as recorded.
    fn finish_halt(&mut self, block: RBlockId, kinds: HaltKinds, origin: SemOrigin<'db>) {
        if self.unwinds {
            self.return_from_halt(block, origin);
            return;
        }
        let mut kinds: Vec<_> = kinds.iter().collect();
        let last = kinds.pop().expect("a halting callee halts");
        if kinds.is_empty() {
            self.halt_as_recorded(block, last, origin);
            return;
        }
        let discr = self.load_record(block, KIND, word_class(), origin);
        let cases = kinds
            .into_iter()
            .map(|kind| {
                let target = self.new_block(origin);
                self.halt_as_recorded(target, kind, origin);
                (uint_scalar(256, kind as u64), target)
            })
            .collect();
        let default = self.new_block(origin);
        self.halt_as_recorded(default, last, origin);
        self.terminate(
            block,
            RTerminator::SwitchScalar {
                discr,
                cases,
                default,
            },
            origin,
        );
    }

    fn halt_as_recorded(&mut self, block: RBlockId, kind: HaltKind, origin: SemOrigin<'db>) {
        let terminator = match kind {
            HaltKind::Return => {
                let pointer = RuntimeClass::Scalar(pointer_class(self.db, AddressSpace::Memory));
                let offset = self.load_record(block, DATA, pointer, origin);
                let len = self.load_record(block, WORD, word_class(), origin);
                RTerminator::ReturnData { offset, len }
            }
            HaltKind::Stop => RTerminator::Stop,
            HaltKind::SelfDestruct => RTerminator::SelfDestruct {
                beneficiary: self.load_record(block, WORD, word_class(), origin),
            },
        };
        self.terminate(block, terminator, origin);
    }

    /// Records the halt `terminator` and returns, in an unwinding body. The
    /// return data is copied to an allocation of its own, which outlives
    /// the frames the unwinding leaves.
    fn record_halt(
        &mut self,
        block: RBlockId,
        terminator: RTerminator<'db>,
        origin: SemOrigin<'db>,
    ) {
        let kind = HaltKind::of(&terminator).expect("a successful halt");
        match terminator {
            RTerminator::ReturnData { offset, len } => {
                let data = self.assign(
                    block,
                    RuntimeClass::Scalar(pointer_class(self.db, AddressSpace::Memory)),
                    RExpr::Builtin(RuntimeBuiltin::Malloc { size: len }),
                    origin,
                );
                let sink = self.local(RuntimeCarrier::Erased, RuntimeLocalRoot::None);
                self.push(
                    block,
                    RStmt::Assign {
                        dst: sink,
                        expr: RExpr::Builtin(RuntimeBuiltin::Mcopy {
                            dst: data,
                            src: offset,
                            len,
                        }),
                    },
                    origin,
                );
                self.store_record(block, DATA, data, origin);
                self.store_record(block, WORD, len, origin);
            }
            RTerminator::SelfDestruct { beneficiary } => {
                self.store_record(block, WORD, beneficiary, origin);
            }
            _ => {}
        }
        let kind = self.word_const(block, kind as u64, origin);
        self.store_record(block, KIND, kind, origin);
        self.return_from_halt(block, origin);
    }

    /// Returns from an unwinding body that halted: the caller only reads
    /// the halt record.
    fn return_from_halt(&mut self, block: RBlockId, origin: SemOrigin<'db>) {
        let value =
            self.body.signature.ret.clone().map(|class| {
                self.assign(block, class.clone(), RExpr::Placeholder { class }, origin)
            });
        self.terminate(block, RTerminator::Return(value), origin);
    }

    fn rewrite_block(&mut self, block: RBlockId, held: Option<Held>) {
        let stmts = std::mem::take(&mut self.body.blocks[block.index()].stmts);
        let origins = std::mem::take(&mut self.body.stmt_origins[block.index()]);
        let terminator = std::mem::replace(
            &mut self.body.blocks[block.index()].terminator,
            RTerminator::Trap,
        );
        let terminator_origin = self.body.terminator_origins[block.index()];
        let Some(mut held) = held else {
            // Unreachable: only the marks go.
            for (stmt, origin) in stmts.into_iter().zip(origins) {
                if !matches!(stmt, RStmt::HoldLane { .. } | RStmt::ReleaseLane { .. }) {
                    self.push(block, stmt, origin);
                }
            }
            self.terminate(block, terminator, terminator_origin);
            return;
        };
        let mut current = block;
        for (stmt, origin) in stmts.into_iter().zip(origins) {
            match stmt {
                RStmt::HoldLane { copy, .. } | RStmt::ReleaseLane { copy } => {
                    let holds = matches!(stmt, RStmt::HoldLane { .. });
                    held.apply(&stmt);
                    if let Some(&flag) = self.flags.get(&copy) {
                        self.push(
                            current,
                            RStmt::Assign {
                                dst: flag,
                                expr: RExpr::ConstScalar(ConstScalar::Bool(holds)),
                            },
                            origin,
                        );
                    }
                }
                RStmt::Assign {
                    dst,
                    expr: RExpr::Call { callee, args },
                } if let Some(kinds) = self.unwound(callee, &held) => {
                    let (callee, args) = self.unwinding_call(current, callee, &args, origin);
                    self.push(
                        current,
                        RStmt::Assign {
                            dst,
                            expr: RExpr::Call { callee, args },
                        },
                        origin,
                    );
                    let kind = self.load_record(current, KIND, word_class(), origin);
                    let zero = self.word_const(current, 0, origin);
                    let cond = self.assign(
                        current,
                        bool_class(),
                        RExpr::Binary {
                            op: BinOp::Comp(CompBinOp::NotEq),
                            lhs: kind,
                            rhs: zero,
                        },
                        origin,
                    );
                    let (halted, next) = (self.new_block(origin), self.new_block(origin));
                    self.terminate(
                        current,
                        RTerminator::Branch {
                            cond,
                            then_bb: halted,
                            else_bb: next,
                        },
                        origin,
                    );
                    let halted = self.flush(halted, &held, origin);
                    self.finish_halt(halted, kinds, origin);
                    current = next;
                }
                stmt => self.push(current, stmt, origin),
            }
        }
        let origin = terminator_origin;
        match terminator {
            RTerminator::TerminalCall { callee, args }
                if let Some(kinds) = self.unwound(callee, &held) =>
            {
                let (callee, args) = self.unwinding_call(current, callee, &args, origin);
                let dst = match runtime_declaration_signature(self.db, callee.key(self.db)).ret {
                    Some(class) => self.value(class),
                    None => self.local(RuntimeCarrier::Erased, RuntimeLocalRoot::None),
                };
                self.push(
                    current,
                    RStmt::Assign {
                        dst,
                        expr: RExpr::Call { callee, args },
                    },
                    origin,
                );
                let current = self.flush(current, &held, origin);
                self.finish_halt(current, kinds, origin);
            }
            terminator if self.handles_halt(&terminator, &held) => {
                let current = self.flush(current, &held, origin);
                if self.unwinds {
                    self.record_halt(current, terminator, origin);
                } else {
                    self.terminate(current, terminator, origin);
                }
            }
            terminator => self.terminate(current, terminator, origin),
        }
    }
}
