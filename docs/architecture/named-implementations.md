# Named implementations architecture

A named implementation is a trait implementation with a name,
`impl Price for Item as Wholesale`, that ordinary lookup never selects. A
`with` operand makes it a candidate inside a block
([language](../language/named-implementations.md)). The compiler adds no
second resolver: the open choices are an input to the trait solver's proof
construction, and trait evidence carries the result to calls, instances and
runtime lowering.

## Names

`ImplTraitModifiers` (`hir_def::item`) holds a header's `unsafe`, its name
and the name's visibility. The name is an item name in its own domain,
`NameDomain::IMPL`, reached through an `EdgeKind::Impl` edge of the scope
graph, so it never collides with a type or a value. `TraitEnv` leaves named
implementations out of ordinary lookup. `resolve_impl_name`
(`path_resolver`) resolves a `with` operand in that domain and checks the
name's visibility. An operand, bare or qualified, that also resolves to a
visible value is reported as ambiguous rather than ranked. The
definition-conflict pass (`DefConflictAnalysisPass`, `ty`) gives these names
their own domain, so two named implementations with one name in one module,
or in one block of a body, are reported where they are declared.

`unchoosable_named_trait` (`trait_def`) refuses a named implementation that no
use can choose. The compiler reads these traits only by presence.

## Choices

`ChosenImpls` (`trait_resolution`) lists the implementations opened by the
enclosing `with` blocks, outermost first. `check_with` (`ty_check::expr`)
extends it at each operand and restores it at the end of the block. It is
input to proof construction, never an environment attached to a callee:
the solver prefers the nearest matching choice for a goal and for the
conditions of the implementations it selects, and a choice whose
conditions fail is an error, not a reason to try the ordinary
implementation. An operand whose implementation could answer a goal that
an earlier operand of the same list answers
(`common_choice_goal`, the header test of `matching_choice`) is
reported (`DuplicateWithChoice`) and not added, so the order inside one
list never decides a selection. The overlap check compares structural
arguments and resolves projections using the caller's bounds. If unresolved
outputs could still make both headers match, it reports a possible overlap.

Normalize associated types using the arguments and evidence available at the
goal. A candidate's own conditions are proof obligations after header
selection. They cannot make a different concrete header match.

A qualified operand `<Type as Trait>` opens the ordinary implementation
that its header alone selects (`resolve_ordinary_trait_impl_instance`).
Its conditions are registered as obligations once the whole list is read,
with the list's choices except the opening itself. A call or obligation that
is decided after the surrounding expression keeps the choices in scope where
it was written.

Method selection compares trait instances after evaluating their constants.
A chosen or opened implementation may hold an array length as a value while
the ordinary implementation still holds it as an expression. Both forms name
the same method candidate when the lengths agree.

## Evidence

`Evidence` (`ty::evidence`) is a proof: an implementation with its
instantiation for the goal and a proof of each condition, an assumption
slot of the declaration that owns it (`BoundSlotRef`), or the deferred
Solidity codec leaf described below. A `Callable` carries the proofs of the
bounds its callee declares (`BoundEvidence`, numbered by the callee's
`BoundSchemaId`) and, for open effect rows, `RowEvidence` addressed by the
component's structural path, which stays stable when substitution expands
an abstract row. A closure captures the dictionaries of the declaration it
is written in.

Transport never selects an implementation. Substitution and rebasing move
a proof between declarations; completion at an instance boundary either
finishes a proof or reports an error, so a failed or unfinished proof never
becomes an absent one that another implementation could answer. A
supertrait dictionary is a projection of its subtrait's proof, and every
path to one bound must give the same dictionary.

One function, `received`, fills and checks the proofs a body receives: the
proofs of an instance (`instance_evidence`), of a call's declared bounds
(`declared_bound_evidence_from`) and of a selected implementation's conditions
(`received_conditions`). A goal of the body is a slot of its layout (the bounds
it opens, `opened_bounds`) or a concrete component that the slots'
dictionaries hold (`concrete_components`). Two goals are one goal when
`same_identity` equates them in the coordinates of the declaration, and each
goal receives one dictionary. Two dictionaries agree when `same_dictionary`
says so or when neither selects a named implementation, so nothing is compared
where no proof supplied to the body selects one. A component below a concrete
bound the walk added is fixed by the implementation that defines the associated
type, read through ordinary normalization (`component_walk`); the walk of
assumptions (`extend_all_bounds`) reads no implementation, because the
implementations are found by lowering headers that read it.

## Instances

`SemanticInstanceKey::of` keeps the supplied proofs that select a named
implementation or supply an effect row. Ordinary proofs follow from the
argument types and are rebuilt by `instance_evidence`; assumption leaves
are named by owner and slot. The stable keys of runtime lowering
(`mir::runtime::stable_key`) include the same proofs, so one function under
two choices becomes two instances.

## Associated outputs

Types normalize through ordinary implementations only.
`named_output_mismatch` (`trait_def`) requires a source-declared ordinary
implementation covering a named implementation that has associated types,
effect rows or constants, and compares the outputs of overlapping
implementations at their overlap. Constants are compared after evaluation,
and a body that stays deferred by its canonical semantic form. A written
type, alias or anonymous constant whose formation reads a choice is refused
(`type_choice_read`, `const_requirements`).

The same comparison runs at a concrete goal that a proof answers through a
named implementation at any depth (`concrete_goal_output_mismatch`,
`trait_def`). `SemanticInstance::output_agreement_diagnostic` applies it to
every proof a body's uses carry before the body is lowered. A callee's own
uses are checked when the callee is admitted. Where the admission of an
instance fails internally, the first disagreement in the instances it calls
(only those within the instance limits) is reported instead
(`callee_output_disagreement`). The codec proofs that `entry_codec_callees`
completes after admission are checked there.
The other two places that complete a deferred codec proof after admission
(`semantic_callee_key_with_assumptions` and `instance_evidence`) do not
compare outputs. No program that reaches a disagreement there was found.

An overlap used to compare outputs binds the structural arguments first.
It then uses the declared equalities from both implementations to normalize
the projected arguments at that application.

## Storage

`PlaceEntryCalls` (`ty_check`) records the `StateIndex::locate` and
`LaneCodec` callables proved for a storage indexing expression. Access
analysis and runtime lowering consume those calls instead of selecting the
core methods again. Since different dictionaries can map different keys to
one location, the access checker treats entries reached through different
dictionaries as possibly overlapping.

A codec obligation whose answer depends on a generic element is kept as the
`DeferredSolCodec` leaf inside any selected parent proof. Substitution
carries it, and instance finalization completes only the leaf. The solver's
provisional whole-slot codec is never recorded as a selection, because its
premise can change when the element becomes packed.

## Limits

- Type normalization does not read evidence, so a type cannot depend on a
  choice.
- Compiler-generated ordinary implementations do not serve as output
  representatives and are not compared.
- There are no evidence values, `prove` expressions or constraint kinds in
  the language.
- Implied bounds are followed four associated-type levels below a bound that
  an equality fixes, each level fixed by the implementation that defines it.
  Beyond that, a program whose dictionaries involve a named
  implementation is refused (16-0019); a program that involves none is
  unaffected.
- A function item takes the choices in force where it is passed to a callable
  bound, not where the item is written: after
  `let f = with (Alt) { helper<u256> }`, `run(f)` uses the ordinary
  implementation, while `let f = helper<u256>` followed by
  `with (Alt) { run(f) }` uses `Alt`.
- Inside a call cycle, which of several real disagreements in the callees is
  reported can depend on the order in which they are checked.
- The message "no ordinary implementation defines its associated outputs" is
  also given, in the check of a named implementation's header, where the
  ordinary implementation of the goal is ambiguous. The check at a concrete
  goal tells the two apart.
- The runtime lowering error "runtime call requires a selected implementation"
  is not attributed to a callee whose outputs disagree with ordinary
  normalization.
