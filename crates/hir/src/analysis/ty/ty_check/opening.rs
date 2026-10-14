//! Lexical trait slots. Payloads and policy are separate from effect insertion.
use crate::{
    analysis::{
        HirAnalysisDb,
        ty::{
            fold::{TyFoldable, TyFolder},
            trait_def::{ImplApplicationRecord, TraitInstId},
            trait_resolution::TraitSolveCx,
            visitor::{TyVisitable, TyVisitor},
        },
    },
    core::semantic::lexical::LexicalFrames,
    hir_def::ExprId,
};

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, salsa::Update)]
pub enum TraitOpening<'db> {
    Named(ImplApplicationRecord<'db>),
    Evidence {
        operand: ExprId,
        goal: TraitInstId<'db>,
        admission: Option<TraitSolveCx<'db>>,
    },
}

impl<'db> TraitOpening<'db> {
    pub fn goal(self) -> TraitInstId<'db> {
        match self {
            Self::Named(record) => record.goal(),
            Self::Evidence { goal, .. } => goal,
        }
    }
}

impl<'db> TyFoldable<'db> for TraitOpening<'db> {
    fn super_fold_with<F: TyFolder<'db>>(self, db: &'db dyn HirAnalysisDb, folder: &mut F) -> Self {
        match self {
            Self::Named(record) => Self::Named(record.fold_with(db, folder)),
            Self::Evidence {
                operand,
                goal,
                admission,
            } => Self::Evidence {
                operand,
                goal: goal.fold_with(db, folder),
                admission: admission.map(|context| context.fold_with(db, folder)),
            },
        }
    }
}
impl<'db> TyVisitable<'db> for TraitOpening<'db> {
    fn visit_with<V: TyVisitor<'db> + ?Sized>(&self, visitor: &mut V) {
        match self {
            Self::Named(record) => record.visit_with(visitor),
            Self::Evidence {
                goal, admission, ..
            } => {
                goal.visit_with(visitor);
                if let Some(context) = admission {
                    context.visit_with(visitor);
                }
            }
        }
    }
}

pub(super) struct TraitOpenings<'db> {
    frames: LexicalFrames<Vec<TraitOpening<'db>>>,
}
impl<'db> TraitOpenings<'db> {
    pub fn new() -> Self {
        Self {
            frames: LexicalFrames::new(Vec::new()),
        }
    }
    pub fn enter(&mut self) {
        self.frames.enter(Vec::new());
    }
    pub fn leave(&mut self) {
        self.frames.leave();
    }
    pub fn insert(
        &mut self,
        db: &'db dyn HirAnalysisDb,
        opening: TraitOpening<'db>,
    ) -> Result<(), ()> {
        let frame = self.frames.current_mut();
        if frame
            .iter()
            .any(|previous| same_slot(db, previous.goal(), opening.goal()))
        {
            return Err(());
        }
        frame.push(opening);
        Ok(())
    }
    pub fn finalize_evidence(&mut self, operand: ExprId, admission: TraitSolveCx<'db>) {
        for opening in self.frames.current_mut().iter_mut() {
            if let TraitOpening::Evidence {
                operand: source,
                admission: context,
                ..
            } = opening
                && *source == operand
            {
                *context = Some(admission);
                return;
            }
        }
    }
    pub fn lookup(
        &self,
        db: &'db dyn HirAnalysisDb,
        goal: TraitInstId<'db>,
    ) -> Option<TraitOpening<'db>> {
        self.frames
            .inner_first()
            .flat_map(|frame| frame.iter().rev())
            .copied()
            .find(|opening| same_slot(db, opening.goal(), goal))
    }
    pub fn visible(&self, db: &'db dyn HirAnalysisDb) -> Vec<TraitOpening<'db>> {
        let mut visible: Vec<TraitOpening<'db>> = Vec::new();
        for opening in self
            .frames
            .inner_first()
            .flat_map(|frame| frame.iter().rev())
            .copied()
        {
            if !visible
                .iter()
                .any(|previous| same_slot(db, previous.goal(), opening.goal()))
            {
                visible.push(opening);
            }
        }
        visible
    }
}

// Equalities constrain the accepted answer, rather than allocating another slot.
fn same_slot<'db>(db: &'db dyn HirAnalysisDb, a: TraitInstId<'db>, b: TraitInstId<'db>) -> bool {
    a.def(db) == b.def(db) && a.args(db) == b.args(db)
}
