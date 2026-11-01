use std::{collections::HashMap, convert::Infallible};

pub(crate) use item::ItemListScope;
use smallvec::SmallVec;

use self::token_stream::{LexicalToken, TokenStream};
use crate::{ExpectedKind, GreenNode, SyntaxKind, TextRange, syntax_node::SyntaxNode};

pub mod token_stream;

pub use crate::ParseError;
pub use pat::parse_pat;

pub mod attr;
pub mod expr;
pub mod func;
pub mod item;
pub mod lit;
pub mod param;
pub mod pat;
pub mod path;
pub mod stmt;
pub mod struct_;
pub mod type_;
pub mod use_tree;

mod expr_atom;

type Checkpoint = rowan::Checkpoint;

#[derive(Default, Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum RecoveryMode {
    #[default]
    Recover,
    NoRecover,
}

impl RecoveryMode {
    pub fn new(use_recovery: bool) -> Self {
        if use_recovery {
            Self::Recover
        } else {
            Self::NoRecover
        }
    }
}

/// Parser to build a rowan syntax tree.
pub struct Parser<S: TokenStream> {
    /// The tokens to parse, trivia included.
    tokens: Vec<S::Token>,
    /// The index in `tokens` of the next token to bump.
    token_pos: usize,

    builder: rowan::GreenNodeBuilder<'static>,
    parents: Vec<ScopeEntry>,
    errors: Vec<ParseError>,

    /// if `is_newline_trivia` is `true`, `Newline` is also regarded as a trivia
    /// token.
    is_newline_trivia: bool,

    current_pos: rowan::TextSize,
    end_of_prev_token: rowan::TextSize,
    /// The dry run states which holds the each state of the parser when it
    /// enters dry run mode.
    dry_run_states: Vec<DryRunState>,

    /// Outcomes of the speculative probes run so far, see [`Parser::probe`].
    probe_cache: HashMap<(rowan::TextSize, bool, ProbeKind), bool>,
    /// The number of [`Parser::probe`]s currently running.
    probe_depth: usize,

    /// Whether or not to recover from syntax errors automatically.
    recovery_mode: RecoveryMode,
}

/// A speculative parse used only to disambiguate what follows, identified so
/// its outcome can be reused at the same position.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub(crate) enum ProbeKind {
    /// `<Type as Trait>`, as opposed to a comparison or shift.
    QualifiedType,
    /// `<<` opening generic arguments whose first argument is a qualified
    /// path, as opposed to a left shift.
    LShiftOpensGenericArgs,
    /// A path followed by `(`, i.e. a call in const generic argument position.
    ConstCall,
    /// A generic argument list, as opposed to a comparison. `expr_path`
    /// marks a segment of an expression's path (see
    /// `path::PathSegmentScope`).
    GenericArgList { is_expr: bool, expr_path: bool },
}

impl<S: TokenStream> Parser<S> {
    /// Create a parser with the given token stream.
    pub fn new(mut stream: S, recovery_mode: RecoveryMode) -> Self {
        Self {
            tokens: std::iter::from_fn(|| stream.next()).collect(),
            token_pos: 0,
            builder: rowan::GreenNodeBuilder::new(),
            parents: Vec::new(),
            errors: Vec::new(),
            current_pos: rowan::TextSize::from(0),
            end_of_prev_token: rowan::TextSize::from(0),
            is_newline_trivia: true,
            dry_run_states: Vec::new(),
            probe_cache: HashMap::new(),
            probe_depth: 0,
            recovery_mode,
        }
    }

    /// Returns the current token of the parser.
    pub fn current_token(&mut self) -> Option<S::Token> {
        self.peek_non_trivia()
    }

    /// Returns the current non-trivia token kind of the parser.
    pub fn current_kind(&mut self) -> Option<SyntaxKind> {
        self.current_token().map(|tok| tok.syntax_kind())
    }

    pub fn current_kind_same_line(&mut self) -> Option<SyntaxKind> {
        let nt = self.set_newline_as_trivia(false);
        let kind = self.current_kind();
        self.set_newline_as_trivia(nt);
        kind
    }

    pub fn is_ident(&mut self, ident: &str) -> bool {
        self.current_token()
            .map(|tok| tok.syntax_kind() == SyntaxKind::Ident && tok.text() == ident)
            .unwrap_or_default()
    }

    /// Sets the newline kind as trivia if `is_trivia` is `true`. Otherwise, the
    /// newline kind is not regarded as a trivia.
    ///
    /// Returns previous value.
    pub fn set_newline_as_trivia(&mut self, is_trivia: bool) -> bool {
        std::mem::replace(&mut self.is_newline_trivia, is_trivia)
    }

    pub fn bump_continuation_newlines(&mut self) {
        if !self.has_continuation_newline() {
            return;
        }

        let mut saw_newline = false;
        loop {
            match self.peek_raw().map(|tok| tok.syntax_kind()) {
                Some(SyntaxKind::WhiteSpace) => self.bump_raw(),
                Some(SyntaxKind::Newline) => {
                    saw_newline = true;
                    self.bump_raw();
                }
                Some(SyntaxKind::Comment) if saw_newline => self.bump_raw(),
                _ => break,
            }
        }
    }

    /// Finish the parsing and return the GreeNode.
    pub fn finish(self) -> (GreenNode, Vec<ParseError>) {
        debug_assert!(self.parents.is_empty());
        debug_assert!(!self.is_dry_run());

        (self.builder.finish(), self.errors)
    }

    /// Finish the parsing and return the SyntaxNode.
    /// **NOTE:** This method is mainly used for testing.
    pub fn finish_to_node(self) -> (SyntaxNode, Vec<ParseError>) {
        let (green_node, errors) = self.finish();
        (SyntaxNode::new_root(green_node), errors)
    }

    pub fn set_scope_recovery_stack(&mut self, tokens: &[SyntaxKind]) {
        let rec = self.scope_aux_recovery();
        rec.clear();
        rec.extend(tokens.iter().rev().copied());
    }

    pub fn pop_recovery_stack(&mut self) {
        self.scope_aux_recovery().pop();
    }

    fn scope_aux_recovery(&mut self) -> &mut SmallVec<SyntaxKind, 4> {
        &mut self.parents.last_mut().unwrap().aux_recovery_tokens
    }

    pub(super) fn in_scope_set(&self, scopes: &[SyntaxKind]) -> bool {
        self.parents
            .last()
            .is_some_and(|scope| scopes.contains(&scope.kind))
    }

    /// Changes the node kind of the current scope.
    pub(super) fn set_scope_kind(&mut self, kind: SyntaxKind) {
        self.parents.last_mut().unwrap().kind = kind;
    }

    pub(super) fn quote_scope_depth(&self) -> usize {
        self.parents
            .iter()
            .filter(|entry| entry.kind == SyntaxKind::QuoteExpr)
            .count()
    }

    /// Whether a `$` here starts a splice in a slot that only quotes have.
    pub(super) fn at_quote_hole(&mut self) -> bool {
        self.current_kind() == Some(SyntaxKind::Dollar) && self.quote_scope_depth() > 0
    }

    pub fn expect_and_pop_recovery_stack(&mut self) -> Result<(), Recovery<ErrProof>> {
        let current = self.current_kind();
        let r = if current.is_some() && self.scope_aux_recovery().contains(&current.unwrap()) {
            Ok(())
        } else {
            let pos = self.current_pos;
            let (index, unexpected) = self.recover();
            let proof = if unexpected.is_some() {
                ErrProof(())
            } else {
                let err = ParseError::expected(self.scope_aux_recovery(), None, pos);
                self.add_error(err)
            };
            self.allow_local_recovery(Err(Recovery(index, proof)))
        };
        self.pop_recovery_stack();
        r
    }

    pub fn expect(
        &mut self,
        expected: &[SyntaxKind],
        kind: Option<ExpectedKind>,
    ) -> Result<(), Recovery<ErrProof>> {
        let current = self.current_kind();

        let aux = self.scope_aux_recovery();
        let truncate_to = aux.len();
        aux.extend_from_slice(expected);

        let res = if current.is_some() && aux.contains(&current.unwrap()) {
            Ok(())
        } else {
            let pos = self.current_pos;
            let (index, unexpected) = self.recover();
            let proof = if unexpected.is_some() {
                ErrProof(())
            } else {
                self.add_error(ParseError::expected(expected, kind, pos))
            };
            self.pop_recovery_stack();
            self.allow_local_recovery(Err(Recovery(index, proof)))
        };
        self.scope_aux_recovery().truncate(truncate_to);

        res
    }

    /// Adds the `recovery_tokens` as a temporary recovery token set.
    /// These tokens are used as a recovery token set in addition to scope's
    /// recovery token set.
    ///
    /// This is useful when you want to specify auxiliary recovery tokens which
    /// are valid only in a limited part of the scope.
    pub fn with_recovery_tokens<F, R>(&mut self, f: F, recovery_tokens: &[SyntaxKind]) -> R
    where
        F: FnOnce(&mut Self) -> R,
    {
        let truncate_to = self.scope_aux_recovery().len();
        self.scope_aux_recovery().extend_from_slice(recovery_tokens);
        let r = f(self);
        self.scope_aux_recovery().truncate(truncate_to);
        r
    }

    /// Invoke the scope to parse. The scope is wrapped up by the node specified
    /// by the scope.
    ///
    /// # Arguments
    /// * If the `checkpoint` is `Some`, the marked branch is wrapped up by the
    ///   node.
    /// * If the `checkpoint` is `None`, the current branch is wrapped up by the
    ///   node.
    ///
    /// # Returns
    /// * If the parsing succeeds, the first element of the return value is
    ///   `true`. otherwise, the first element is `false`.
    /// * The second element of the return value is the checkpoint of the start
    ///   of the node.
    pub fn parse_cp<T, E>(
        &mut self,
        mut scope: T,
        checkpoint: Option<Checkpoint>,
    ) -> Result<Checkpoint, E>
    where
        T: Parse<Error = E>,
        E: Recoverable,
    {
        let checkpoint = self.enter::<T>(checkpoint);
        let start_checkpoint = self.checkpoint();
        let res = scope.parse(self);
        self.leave(checkpoint);
        let res = self.allow_local_recovery(res);
        res.map(|_| start_checkpoint)
    }

    pub fn parse<T, E>(&mut self, scope: T) -> Result<(), E>
    where
        T: Parse<Error = E>,
        E: Recoverable,
    {
        self.parse_ok(scope).map(|_| ())
    }

    pub fn parse_ok<T, E>(&mut self, mut scope: T) -> Result<bool, E>
    where
        T: Parse<Error = E>,
        E: Recoverable,
    {
        let checkpoint = self.enter::<T>(None);
        let res = scope.parse(self);
        self.leave(checkpoint);
        let ok = res.is_ok();
        let res = self.allow_local_recovery(res);
        res.map(|_| ok)
    }

    pub fn parses_without_error<T, E>(&mut self, mut scope: T) -> bool
    where
        T: Parse<Error = E>,
        E: Recoverable,
    {
        let checkpoint = self.enter::<T>(None);
        let ok = scope.parse(self).is_ok();
        self.leave(checkpoint);
        ok && !self.dry_run_states.last().unwrap().err
    }

    pub fn parse_or_recover<T>(&mut self, scope: T) -> Result<(), Recovery<ErrProof>>
    where
        T: Parse<Error = ParseError>,
    {
        self.or_recover(|parser| parser.parse(scope))
    }

    pub fn or_recover<F>(&mut self, f: F) -> Result<(), Recovery<ErrProof>>
    where
        F: FnOnce(&mut Self) -> Result<(), ParseError>,
    {
        if let Err(err) = f(self) {
            let proof = self.add_error(err);
            self.try_recover().map_err(|r| r.add_err_proof(proof))?;
        }
        Ok(())
    }

    #[doc(hidden)]
    /// Enter the scope and return the checkpoint. The checkpoint branch will be
    /// wrapped up by the scope's node when [`leave`] is called.
    // NOTE: This method is limited to testing and internal usage.
    pub fn enter<T: ParsingScope>(&mut self, checkpoint: Option<Checkpoint>) -> Checkpoint {
        // Ensure the leading trivias are added to the parent node.
        if !self.parents.is_empty() {
            self.bump_trivias();
        }

        self.parents.push(ScopeEntry {
            kind: T::KIND,
            recovery_tokens: T::RECOVERY_TOKENS,
            is_newline_trivia: self.is_newline_trivia,
            is_err: false,
            aux_recovery_tokens: SmallVec::new(),
        });
        // `is_newline_trivia` is always `true` when entering a scope.
        self.is_newline_trivia = true;
        checkpoint.unwrap_or_else(|| self.checkpoint())
    }

    #[doc(hidden)]
    /// Leave the scope and wrap up the checkpoint by the scope's node.
    /// Returns `is_err` value for exited scope.
    // NOTE: This method is limited to testing and internal usage.
    pub fn leave(&mut self, checkpoint: Checkpoint) -> bool {
        let scope = self.parents.pop().unwrap();
        self.is_newline_trivia = scope.is_newline_trivia;

        // Ensure the trailing trivias are added to the current node if the current
        // scope is the root.
        if self.parents.is_empty() {
            self.bump_trivias()
        }

        if !self.is_dry_run() {
            self.builder.start_node_at(checkpoint, scope.kind.into());
            self.builder.finish_node();
        } else {
            self.dry_run_states.last_mut().unwrap().err |= scope.is_err;
        }
        scope.is_err
    }

    pub fn add_error(&mut self, err: ParseError) -> ErrProof {
        self.parents.last_mut().unwrap().is_err = true;
        self.errors.push(err);
        ErrProof(())
    }

    /// Add `msg` as an error to the error list, then bumps consecutive tokens
    /// until a token in the recovery set is found.
    ///
    /// * If checkpoint is `Some`, the marked branch is wrapped up by an error
    ///   node.
    /// * If checkpoint is `None`, the current branch is wrapped up by an error
    ///   node.
    pub fn error_and_recover(&mut self, msg: &str) -> Result<(), Recovery<ErrProof>> {
        let proof = self.add_error(ParseError::Msg(
            msg.into(),
            TextRange::empty(self.end_of_prev_token),
        ));
        self.try_recover().map_err(|r| r.add_err_proof(proof))
    }

    /// Runs a disambiguation probe in dry run mode, reusing the outcome of an
    /// identical probe already run at this position.
    ///
    /// `dry_run` reverts the stream, position, buffered trivia and errors, and
    /// leaves the tree builder untouched, so a probe depends only on the position
    /// it starts from and on whether newlines are trivia there. The input never
    /// changes during a parse, so an outcome stays valid.
    ///
    /// A probe asks whether a construct parses here on its own, so it fails at
    /// its first error and never recovers. Recovery would read past the
    /// construct: `recover` searches the enclosing scopes for a token to stop at,
    /// so its reach depends on where the probe ran from, and with no stopping
    /// token it consumes the rest of the file.
    ///
    /// Nesting is what makes caching worthwhile: each level of
    /// `Wrap<<T as Model>::Point>` probes the positions inside it, and every
    /// enclosing level probes them all again, so without this the same position
    /// is re-parsed a multiple of times per level of depth.
    pub(crate) fn probe<F>(&mut self, kind: ProbeKind, f: F) -> bool
    where
        F: FnOnce(&mut Self) -> bool,
    {
        let key = (self.current_pos, self.is_newline_trivia, kind);
        if let Some(&outcome) = self.probe_cache.get(&key) {
            return outcome;
        }
        self.probe_depth += 1;
        let outcome = self.dry_run(|parser| {
            let err_num = parser.errors.len();
            f(parser) && parser.errors.len() == err_num
        });
        self.probe_depth -= 1;
        self.probe_cache.insert(key, outcome);
        outcome
    }

    /// Runs the parser in the dry run mode.
    ///
    /// Any changes to the parser state will be reverted.
    pub fn dry_run<F, R>(&mut self, f: F) -> R
    where
        F: FnOnce(&mut Self) -> R,
    {
        // Enters the dry run mode.
        self.dry_run_states.push(DryRunState {
            token_pos: self.token_pos,
            pos: self.current_pos,
            end_of_prev_token: self.end_of_prev_token,
            err_num: self.errors.len(),
            err: false,
        });

        let r = f(self);

        // Leaves the dry run mode.
        let state = self.dry_run_states.pop().unwrap();
        self.errors.truncate(state.err_num);
        self.token_pos = state.token_pos;
        self.current_pos = state.pos;
        self.end_of_prev_token = state.end_of_prev_token;

        r
    }

    /// Bumps the current token and its leading trivias.
    pub fn bump(&mut self) {
        // Bump leading trivias.
        self.bump_trivias();

        self.bump_raw();
        self.end_of_prev_token = self.current_pos;
    }

    /// Bumps the current token if the current token is the `expected` kind.
    ///
    /// # Panics
    /// Panics If the current token is not the `expected` kind.
    pub fn bump_expected(&mut self, expected: SyntaxKind) {
        assert_eq!(self.current_kind(), Some(expected), "expected {expected:?}");
        self.bump();
    }

    /// Bumps the current token if the current token is the `expected` kind.
    /// Return `true` if the current token is the `expected` kind.
    pub fn bump_if(&mut self, expected: SyntaxKind) -> bool {
        if self.current_kind() == Some(expected) {
            self.bump();
            true
        } else {
            false
        }
    }

    pub fn find(
        &mut self,
        kind: SyntaxKind,
        err: ExpectedKind,
    ) -> Result<bool, Recovery<ErrProof>> {
        self.scope_aux_recovery().push(kind);
        self.find_and_pop(kind, err)
    }

    pub fn find_and_pop(
        &mut self,
        kind: SyntaxKind,
        err: ExpectedKind,
    ) -> Result<bool, Recovery<ErrProof>> {
        debug_assert_eq!(self.scope_aux_recovery().last(), Some(&kind));

        let r = if self.current_kind() == Some(kind) {
            Ok(true)
        } else {
            let pos = self.current_pos;
            let r = self.try_recover();
            if self.current_kind() == Some(kind) {
                Ok(true)
            } else {
                let proof = self.add_error(ParseError::expected(&[kind], Some(err), pos));
                r.map(|_| false).map_err(|rec| rec.add_err_proof(proof))
            }
        };
        self.scope_aux_recovery().pop();
        r
    }

    pub fn try_recover(&mut self) -> Result<(), Recovery<()>> {
        let (index, _) = self.recover();
        self.allow_local_recovery(Err(Recovery(index, ())))
    }

    /// Consumes tokens until a recovery token is found, and reports an error on
    /// any unexpected tokens.
    /// Returns the index of the scope that matched the recovery token,
    /// and the total string length of the unexpected tokens.
    fn recover(&mut self) -> (Option<ScopeIndex>, Option<rowan::TextSize>) {
        if self.recovery_mode == RecoveryMode::NoRecover || self.probe_depth > 0 {
            return (None, None);
        }

        let mut unexpected = None;
        let mut match_scope_index = None;
        while let Some(kind) = self.current_kind() {
            if let Some((scope_index, _)) = self
                .parents
                .iter()
                .enumerate()
                .rev()
                .find(|(_i, scope)| scope.is_recovery_match(kind))
            {
                match_scope_index = Some(scope_index);
                break;
            }

            if unexpected.is_none() {
                if !self.parents.is_empty() {
                    self.bump_trivias();
                }
                unexpected = Some((self.current_pos, self.checkpoint()));
            }
            self.bump();
        }

        if let Some((start_pos, checkpoint)) = unexpected {
            if !self.is_dry_run() {
                self.builder
                    .start_node_at(checkpoint, SyntaxKind::Error.into());
                self.builder.finish_node();

                self.add_error(ParseError::Unexpected(
                    format!(
                        "unexpected syntax while parsing {}",
                        self.parents.last().unwrap().kind.describe()
                    ),
                    TextRange::new(start_pos, self.current_pos),
                ));
            } else {
                self.dry_run_states.last_mut().unwrap().err = true;
            }
        }

        (
            match_scope_index.map(ScopeIndex),
            unexpected.map(|(start_pos, _)| start_pos),
        )
    }

    fn allow_local_recovery<E: Recoverable>(&self, r: Result<(), E>) -> Result<(), E> {
        match r {
            Ok(()) => Ok(()),
            Err(e) if e.is_local_recovery(self) => Ok(()),
            _ => r,
        }
    }

    fn is_current_scope(&self, index: ScopeIndex) -> bool {
        index.0 + 1 == self.parents.len()
    }

    /// Bumps the current token if the current token is the `expected` kind.
    /// Otherwise, reports an error and proceeds the parser to the recovery
    /// tokens.
    pub fn bump_or_recover(
        &mut self,
        expected: SyntaxKind,
        msg: &str,
    ) -> Result<(), Recovery<ErrProof>> {
        if !self.bump_if(expected) {
            let proof = self.add_error(ParseError::Msg(
                msg.into(),
                TextRange::empty(self.current_pos),
            ));
            self.try_recover().map_err(|r| r.add_err_proof(proof))
        } else {
            Ok(())
        }
    }

    fn checkpoint(&mut self) -> Checkpoint {
        self.builder.checkpoint()
    }

    /// A checkpoint for a node that starts at the next token, after the
    /// leading trivias, which belong to the parent node as `enter` adds them.
    fn checkpoint_after_trivias(&mut self) -> Checkpoint {
        if !self.parents.is_empty() {
            self.bump_trivias();
        }
        self.checkpoint()
    }

    /// Bumps the current token and
    /// current branch.
    fn bump_raw(&mut self) {
        let tok = &self.tokens[self.token_pos];
        self.token_pos += 1;

        self.current_pos += rowan::TextSize::of(tok.text());
        if self.dry_run_states.is_empty() {
            self.builder.token(tok.syntax_kind().into(), tok.text());
        }
    }

    fn bump_trivias(&mut self) {
        // Bump trivias.
        loop {
            match self.peek_raw() {
                Some(tok) if self.is_trivia(tok.syntax_kind()) => self.bump_raw(),
                _ => break,
            }
        }
    }

    /// The kinds of the tokens that have not been bumped yet.
    fn remaining_kinds(&self) -> impl Iterator<Item = SyntaxKind> + '_ {
        self.tokens[self.token_pos..]
            .iter()
            .map(LexicalToken::syntax_kind)
    }

    /// Peek the next non-trivia token.
    fn peek_non_trivia(&self) -> Option<S::Token> {
        self.tokens[self.token_pos..]
            .iter()
            .find(|tok| !self.is_trivia(tok.syntax_kind()))
            .cloned()
    }

    fn peek_raw(&self) -> Option<S::Token> {
        self.tokens.get(self.token_pos).cloned()
    }

    fn has_continuation_newline(&self) -> bool {
        self.remaining_kinds()
            .find(|&kind| kind != SyntaxKind::WhiteSpace)
            == Some(SyntaxKind::Newline)
    }

    /// Skip trivias (and newlines), then peek the next three tokens.
    pub fn peek_three(&self) -> (Option<SyntaxKind>, Option<SyntaxKind>, Option<SyntaxKind>) {
        let mut kinds = self
            .remaining_kinds()
            .skip_while(|&kind| kind.is_trivia() || kind == SyntaxKind::Newline);
        (kinds.next(), kinds.next(), kinds.next())
    }

    /// Skip leading trivias, then peek the next two raw tokens.
    pub fn peek_two(&self) -> (Option<SyntaxKind>, Option<SyntaxKind>) {
        let (a, b, _) = self.peek_three();
        (a, b)
    }

    /// Peek up to `n` non-trivia tokens, skipping trivia before and between
    /// tokens.
    pub fn peek_n_non_trivia(&self, n: usize) -> SmallVec<SyntaxKind, 4> {
        self.remaining_kinds()
            .filter(|&kind| !self.is_trivia(kind))
            .take(n)
            .collect()
    }

    /// Add the `msg` to the error list, at `current_pos`.
    fn error(&mut self, msg: &str) -> ErrProof {
        let pos = self.current_pos;
        self.errors
            .push(ParseError::Msg(msg.into(), TextRange::new(pos, pos)));
        ErrProof(())
    }

    /// Add the `msg` to the error list, on `current_token()`.
    /// Bumps trivias.
    fn error_msg_on_current_token(&mut self, msg: &str) -> ErrProof {
        self.bump_trivias();
        let start = self.current_pos;
        let end = if let Some(current_token) = self.current_token() {
            start + current_token.text_size()
        } else {
            start
        };

        self.add_error(ParseError::Msg(msg.into(), TextRange::new(start, end)))
    }

    /// Wrap the current token in a `SyntaxKind::Error`, and add a
    /// `ParseError::Unexpected`.
    fn unexpected_token_error(&mut self, msg: String) -> ErrProof {
        let checkpoint = self.enter::<ErrorScope>(None);

        let start_pos = self.current_pos;
        self.bump();

        let proof = self.add_error(ParseError::Unexpected(
            msg,
            TextRange::new(start_pos, self.current_pos),
        ));
        self.leave(checkpoint);
        proof
    }

    /// Returns `true` if the parser is in the dry run mode.
    fn is_dry_run(&self) -> bool {
        !self.dry_run_states.is_empty()
    }

    fn is_trivia(&self, kind: SyntaxKind) -> bool {
        kind.is_trivia() || (self.is_newline_trivia && kind == SyntaxKind::Newline)
    }
}

pub trait ParsingScope {
    /// The kind of the node the scope wraps up, unless its `parse` changes it
    /// with [`Parser::set_scope_kind`].
    const KIND: SyntaxKind;

    /// The tokens that stop error recovery in the scope.
    const RECOVERY_TOKENS: &'static [SyntaxKind];
}

pub trait Parse: ParsingScope {
    type Error;
    fn parse<S: TokenStream>(&mut self, parser: &mut Parser<S>) -> Result<(), Self::Error>;
}

pub trait ParseInfalible: ParsingScope {
    fn parse<S: TokenStream>(&mut self, parser: &mut Parser<S>);
}

#[derive(Debug, Copy, Clone)]
pub struct ScopeIndex(usize);
#[derive(Debug, Copy, Clone)]
pub struct Recovery<T>(Option<ScopeIndex>, T);
impl Recovery<()> {
    pub fn add_err_proof(self, proof: ErrProof) -> Recovery<ErrProof> {
        Recovery(self.0, proof)
    }
}

#[derive(Debug)]
pub struct ErrProof(());

pub trait Recoverable {
    fn is_local_recovery<S: TokenStream>(&self, _parser: &Parser<S>) -> bool {
        false
    }
}
impl Recoverable for ParseError {}
impl Recoverable for Infallible {}
impl<T> Recoverable for Recovery<T> {
    fn is_local_recovery<S: TokenStream>(&self, parser: &Parser<S>) -> bool {
        self.0.as_ref().is_some_and(|i| parser.is_current_scope(*i))
    }
}

impl From<Infallible> for ErrProof {
    fn from(_: Infallible) -> ErrProof {
        ErrProof(())
    }
}

impl From<Recovery<Infallible>> for Recovery<ErrProof> {
    fn from(recovery: Recovery<Infallible>) -> Self {
        Self(recovery.0, recovery.1.into())
    }
}

struct DryRunState {
    /// The token index when the dry run started.
    token_pos: usize,
    /// The text position is the position when the dry run started.
    pos: rowan::TextSize,
    end_of_prev_token: rowan::TextSize,
    /// The number of errors when the dry run started.
    err_num: usize,
    err: bool,
}

#[derive(Debug)]
struct ScopeEntry {
    kind: SyntaxKind,
    recovery_tokens: &'static [SyntaxKind],
    is_newline_trivia: bool,
    is_err: bool,
    aux_recovery_tokens: SmallVec<SyntaxKind, 4>,
}
impl ScopeEntry {
    fn is_recovery_match(&self, kind: SyntaxKind) -> bool {
        self.recovery_tokens.contains(&kind) || self.aux_recovery_tokens.contains(&kind)
    }
}

trait TextSize {
    fn text_size(&self) -> rowan::TextSize;
}

impl<T> TextSize for T
where
    T: LexicalToken,
{
    fn text_size(&self) -> rowan::TextSize {
        rowan::TextSize::of(self.text())
    }
}

define_scope! { ErrorScope, Error }
define_scope! { pub RootScope, Root }

macro_rules! define_scope {
    (
        $(#[$attrs: meta])*
        $visibility: vis $scope_name: ident $({ $($field: ident: $ty: ty),* })?,
        $kind: path
    ) => {
        crate::parser::define_scope! {
            $(#[$attrs])* $visibility $scope_name $({ $($field: $ty),* })?, $kind, ()
        }
    };

    (
        $(#[$attrs: meta])*
        $visibility: vis $scope_name: ident $({ $($field: ident: $ty: ty),* })?,
        $kind: path,
        ($($recoveries: path), *)
    ) => {
        $(#[$attrs])*
        #[derive(Debug, Clone, Default)]
        $visibility struct $scope_name {
            $($($field: $ty),*)?
        }
        impl $scope_name {
            #[allow(unused)]
            $visibility fn new($($($field: $ty),*)?) -> Self {
                Self { $($($field),*)? }
            }
        }
        impl crate::parser::ParsingScope for $scope_name {
            const KIND: crate::SyntaxKind = {
                #[allow(unused_imports)]
                use crate::SyntaxKind::*;
                $kind
            };
            const RECOVERY_TOKENS: &'static [crate::SyntaxKind] = {
                #[allow(unused_imports)]
                use crate::SyntaxKind::*;
                &[$($recoveries), *]
            };
        }
    };
}

use define_scope;

/// Parse a comma-separated list of elements, with trailing commas allowed.
/// Panics if `parser.current_kind() != Some(brackets.0)`
fn parse_list<S: TokenStream, F>(
    parser: &mut Parser<S>,
    newline_delim: bool,
    list_kind: SyntaxKind,
    brackets: (SyntaxKind, SyntaxKind),
    element: F,
) -> Result<(), Recovery<ErrProof>>
where
    F: Fn(&mut Parser<S>) -> Result<(), Recovery<ErrProof>>,
{
    parser.bump_expected(brackets.0);

    let expected_closing_bracket = Some(ExpectedKind::ClosingBracket {
        bracket: brackets.1,
        parent: list_kind,
    });

    loop {
        if parser.bump_if(brackets.1) {
            return Ok(());
        }

        element(parser)?;

        if parser.current_kind() != Some(SyntaxKind::Comma)
            && parser.current_kind() != Some(brackets.1)
        {
            // Recover gracefully if list elements are separated by newline instead of comma
            let nt = parser.set_newline_as_trivia(false);
            let newline = parser.current_kind() == Some(SyntaxKind::Newline) || {
                parser.with_recovery_tokens(
                    |parser| {
                        let pos = parser.current_pos;
                        let (index, unexpected) = parser.recover();
                        if unexpected.is_none() {
                            parser.add_error(ParseError::expected(
                                &[brackets.1, SyntaxKind::Comma],
                                expected_closing_bracket,
                                pos,
                            ));
                        }
                        parser.allow_local_recovery(Err(Recovery(index, ErrProof(()))))
                    },
                    &[SyntaxKind::Newline, SyntaxKind::Comma, brackets.1],
                )?;
                parser.current_kind() == Some(SyntaxKind::Newline)
            };
            parser.set_newline_as_trivia(nt);

            if newline {
                parser.add_error(ParseError::expected(
                    &[brackets.1, SyntaxKind::Comma],
                    expected_closing_bracket,
                    parser.current_pos,
                ));
                if !newline_delim {
                    return Ok(());
                }
            } else {
                parser.expect(&[brackets.1, SyntaxKind::Comma], expected_closing_bracket)?;
                if !parser.bump_if(SyntaxKind::Comma) {
                    break;
                }
            }
        } else if !parser.bump_if(SyntaxKind::Comma) {
            parser.expect(&[brackets.1], expected_closing_bracket)?;
            break;
        }
    }
    parser.bump_expected(brackets.1);

    Ok(())
}
