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
- Inside a block, a generic function whose own bound fixes a bounded associated
  type to a concrete type (`C: Collection<Cursor = usize>`) is refused, because
  that bound's implied slot is not kept in the function's assumptions.
