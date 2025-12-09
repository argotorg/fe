use std::{collections::VecDeque, convert::Infallible};

pub(crate) use item::ItemListScope;
use smallvec::SmallVec;

use self::token_stream::{BackTrackableTokenStream, LexicalToken, TokenStream};
use crate::{ExpectedKind, GreenNode, ParseError, SyntaxKind, TextRange, syntax_node::SyntaxNode};

pub mod token_stream;

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

/// Parser to build a rowan syntax tree.
pub struct Parser<S: TokenStream> {
    /// Token stream to parse.
    stream: BackTrackableTokenStream<S>,

    builder: rowan::GreenNodeBuilder<'static>,
    parents: Vec<ScopeEntry>,
    errors: Vec<ParseError>,

    next_trivias: VecDeque<S::Token>,
    /// if `is_newline_trivia` is `true`, `Newline` is also regarded as a trivia
    /// token.
    is_newline_trivia: bool,

    current_pos: rowan::TextSize,
    end_of_prev_token: rowan::TextSize,
    /// The dry run states which holds the each state of the parser when it
    /// enters dry run mode.
    dry_run_states: Vec<DryRunState<S>>,
}

impl<S: TokenStream> Parser<S> {
    /// Create a parser with the given token stream.
    pub fn new(stream: S) -> Self {
        Self {
            stream: BackTrackableTokenStream::new(stream),
            builder: rowan::GreenNodeBuilder::new(),
            parents: Vec::new(),
            errors: Vec::new(),
            current_pos: rowan::TextSize::from(0),
            end_of_prev_token: rowan::TextSize::from(0),
            is_newline_trivia: true,
            next_trivias: VecDeque::new(),
            dry_run_states: Vec::new(),
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

    /// Sets the newline kind as trivia if `is_trivia` is `true`. Otherwise, the
    /// newline kind is not regarded as a trivia.
    ///
    /// Returns previous value.
    pub fn set_newline_as_trivia(&mut self, is_trivia: bool) -> bool {
        std::mem::replace(&mut self.is_newline_trivia, is_trivia)
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
        T: Parse<Error = E> + 'static,
        E: Recoverable,
    {
        let checkpoint = self.enter(scope.clone(), checkpoint);
        let start_checkpoint = self.checkpoint();
        let res = scope.parse(self);
        self.leave(checkpoint);
        let res = self.allow_local_recovery(res);
        res.map(|_| start_checkpoint)
    }

    pub fn parse<T, E>(&mut self, scope: T) -> Result<(), E>
    where
        T: Parse<Error = E> + 'static,
        E: Recoverable,
    {
        self.parse_ok(scope).map(|_| ())
    }

    pub fn parse_ok<T, E>(&mut self, mut scope: T) -> Result<bool, E>
    where
        T: Parse<Error = E> + 'static,
        E: Recoverable,
    {
        let checkpoint = self.enter(scope.clone(), None);
        let res = scope.parse(self);
        self.leave(checkpoint);
        let ok = res.is_ok();
        let res = self.allow_local_recovery(res);
        res.map(|_| ok)
    }

    pub fn parses_without_error<T, E>(&mut self, mut scope: T) -> bool
    where
        T: Parse<Error = E> + 'static,
        E: Recoverable,
    {
        let checkpoint = self.enter(scope.clone(), None);
        let ok = scope.parse(self).is_ok();
        self.leave(checkpoint);
        ok && !self.dry_run_states.last().unwrap().err
    }

    pub fn parse_or_recover<T>(&mut self, scope: T) -> Result<(), Recovery<ErrProof>>
    where
        T: Parse<Error = ParseError> + 'static,
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
    pub fn enter<T>(&mut self, scope: T, checkpoint: Option<Checkpoint>) -> Checkpoint
    where
        T: ParsingScope + 'static,
    {
        // Ensure the leading trivias are added to the parent node.
        if !self.parents.is_empty() {
            self.bump_trivias();
        }

        self.parents
            .push(ScopeEntry::new(Box::new(scope), self.is_newline_trivia));
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
            self.builder
                .start_node_at(checkpoint, scope.scope.syntax_kind().into());
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

    /// Runs the parser in the dry run mode.
    ///
    /// Any changes to the parser state will be reverted.
    pub fn dry_run<F, R>(&mut self, f: F) -> R
    where
        F: FnOnce(&mut Self) -> R,
    {
        // Enters the dry run mode.
        self.stream.set_bt_point();
        self.dry_run_states.push(DryRunState {
            pos: self.current_pos,
            end_of_prev_token: self.end_of_prev_token,
            err_num: self.errors.len(),
            next_trivias: self.next_trivias.clone(),
            err: false,
        });

        let r = f(self);

        // Leaves the dry run mode.
        self.stream.backtrack();
        let state = self.dry_run_states.pop().unwrap();
        self.errors.truncate(state.err_num);
        self.current_pos = state.pos;
        self.end_of_prev_token = state.end_of_prev_token;
        self.next_trivias = state.next_trivias;

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
                        self.parents.last().unwrap().scope.syntax_kind().describe()
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

    /// Bumps the current token and
    /// current branch.
    fn bump_raw(&mut self) {
        let tok = match self.next_trivias.pop_front() {
            Some(tok) => tok,
            None => self.stream.next().unwrap(),
        };

        self.current_pos += rowan::TextSize::of(tok.text());
        if !self.is_dry_run() {
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

    /// Peek the next non-trivia token.
    fn peek_non_trivia(&mut self) -> Option<S::Token> {
        if !self.is_newline_trivia {
            for tok in &self.next_trivias {
                if tok.syntax_kind() == SyntaxKind::Newline {
                    return Some(tok.clone());
                }
            }
        }

        while let Some(next) = self.stream.peek().map(|tok| tok.syntax_kind()) {
            if self.is_trivia(next) {
                let next = self.stream.next().unwrap();
                self.next_trivias.push_back(next);
                continue;
            } else {
                return self.stream.peek().cloned();
            }
        }

        None
    }

    fn peek_raw(&mut self) -> Option<S::Token> {
        if let Some(tok) = self.next_trivias.front() {
            Some(tok.clone())
        } else {
            self.stream.peek().cloned()
        }
    }

    /// Skip trivias (and newlines), then peek the next three tokens.
    pub fn peek_three(&mut self) -> (Option<SyntaxKind>, Option<SyntaxKind>, Option<SyntaxKind>) {
        self.stream.set_bt_point();

        while let Some(next) = self.stream.peek().map(|tok| tok.syntax_kind()) {
            if !(next.is_trivia() || next == SyntaxKind::Newline) {
                break;
            }
            self.stream.next();
        }

        let tokens = (
            self.stream.next().map(|t| t.syntax_kind()),
            self.stream.next().map(|t| t.syntax_kind()),
            self.stream.next().map(|t| t.syntax_kind()),
        );

        self.stream.backtrack();
        tokens
    }

    /// Skip trivias, then peek the next two tokens.
    pub fn peek_two(&mut self) -> (Option<SyntaxKind>, Option<SyntaxKind>) {
        let (a, b, _) = self.peek_three();
        (a, b)
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
        let checkpoint = self.enter(ErrorScope::default(), None);

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
    /// Returns the recovery method of the current scope.
    fn recovery_tokens(&self) -> &[SyntaxKind];

    fn syntax_kind(&self) -> SyntaxKind;
}

pub trait Parse: ParsingScope + Clone {
    type Error;
    fn parse<S: TokenStream>(&mut self, parser: &mut Parser<S>) -> Result<(), Self::Error>;
}

pub trait ParseInfalible: ParsingScope + Clone {
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
        self.0
            .as_ref()
            .map(|i| parser.is_current_scope(*i))
            .unwrap_or(false)
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

struct DryRunState<S: TokenStream> {
    /// The text position is the position when the dry run started.
    pos: rowan::TextSize,
    end_of_prev_token: rowan::TextSize,
    /// The number of errors when the dry run started.
    err_num: usize,
    /// The stored trivias when the dry run started.
    next_trivias: VecDeque<S::Token>,
    err: bool,
}

struct ScopeEntry {
    scope: Box<dyn ParsingScope>,
    is_newline_trivia: bool,
    is_err: bool,
    aux_recovery_tokens: SmallVec<SyntaxKind, 4>,
}
impl ScopeEntry {
    fn new(scope: Box<dyn ParsingScope>, is_newline_trivia: bool) -> Self {
        Self {
            scope,
            is_newline_trivia,
            is_err: false,
            aux_recovery_tokens: SmallVec::new(),
        }
    }

    fn is_recovery_match(&self, kind: SyntaxKind) -> bool {
        self.scope.recovery_tokens().contains(&kind) || self.aux_recovery_tokens.contains(&kind)
    }
}

impl std::fmt::Debug for ScopeEntry {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("ScopeEntry")
            .field("scope", &self.scope.syntax_kind())
            .field("is_newline_trivia", &self.is_newline_trivia)
            .field("is_err", &self.is_err)
            .field("aux_recovery_tokens", &self.aux_recovery_tokens)
            .finish()
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
        crate::parser::define_scope_struct! {$visibility $scope_name {$($($field: $ty), *)?}, $kind}
        impl crate::parser::ParsingScope for $scope_name {
            fn recovery_tokens(&self) -> &[crate::SyntaxKind] {
                &[]
            }

            fn syntax_kind(&self) -> crate::SyntaxKind {
                self.__inner.get()
            }
        }
    };

    (
        $(#[$attrs: meta])*
        $visibility: vis $scope_name: ident $({ $($field: ident: $ty: ty),* })?,
        $kind: path,
        ($($recoveries: path), *)
    ) => {
        crate::parser::define_scope_struct! {$visibility $scope_name {$($($field: $ty), *)?}, $kind}

        impl crate::parser::ParsingScope for $scope_name {
            fn recovery_tokens(&self) -> &[crate::SyntaxKind] {
                lazy_static::lazy_static! {
                    pub(super) static ref RECOVERY_TOKENS: smallvec::SmallVec<SyntaxKind, 4> = {
                        #[allow(unused)]
                        use crate::SyntaxKind::*;
                        smallvec::smallvec![$($recoveries), *]
                    };
                }

                &RECOVERY_TOKENS
            }

            fn syntax_kind(&self) -> crate::SyntaxKind {
                self.__inner.get()
            }
        }
    };
}

macro_rules! define_scope_struct {
    (
        $(#[$attrs: meta])*
        $visibility: vis $scope_name: ident { $($field: ident: $ty: ty),* },
        $kind: path
    ) => {
        $(#[$attrs])*
        #[derive(Debug, Clone)]
        $visibility struct $scope_name {
            __inner: std::rc::Rc<std::cell::Cell<crate::SyntaxKind>>,
            $($field: $ty),*
        }
        impl $scope_name {
            #[allow(unused)]
            $visibility fn new($($field: $ty),*) -> Self {
                use crate::SyntaxKind::*;
                Self {
                    $($field,)*
                    __inner: std::cell::Cell::new($kind).into(),
                }
            }
            #[allow(unused)]
            fn set_kind(&mut self, kind: crate::SyntaxKind) {
                self.__inner.set(kind);
            }
        }
        impl Default for $scope_name {
            fn default() -> Self {
                #[allow(unused_imports)]
                use crate::SyntaxKind::*;
                Self {
                    __inner: std::cell::Cell::new($kind).into(),
                    $($field: Default::default()),*
                }
            }
        }
    };
}

use define_scope;
#[doc(hidden)]
use define_scope_struct;

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
