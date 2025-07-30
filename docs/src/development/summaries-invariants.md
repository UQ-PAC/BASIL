# Abstract interpretation with predicates

## Predicates

- see [src/main/scala/analysis/procedure_summaries/Predicate.scala](https://github.com/UQ-PAC/BASIL/blob/main/src/main/scala/analysis/procedure_summaries/Predicate.scala)

Predicates encode semantic information about program states.
They are distinct to IR boolean expressions as they can explicitly encode information about the gammas of variables.
The intended use of predicates is for representing the result of static analyses used for creating assertions, assumptions, requires clauses or ensures clauses in generated boogie code.
As such, predicates can be translated into boogie expressions.

## Predicate domains

Predicate encoding domains (defined in [src/main/scala/analysis/procedure_summaries/Predicate.scala](https://github.com/UQ-PAC/BASIL/blob/main/src/main/scala/analysis/procedure_summaries/Predicate.scala)) are abstract domains which can represent their results as predicates.
```scala
trait PredicateEncodingDomain[L] extends AbstractDomain[L] {
  def toPred(x: L): Predicate
  // ...
}
```
There is a necessary soundness condition so that the generated predicates are compatable with the transfer function of a domain.
- For forward analyses, we require that applying `toPred` then `sp` is stronger than applying the transfer function then `toPred`.
- For backward analyses, we require that applying `toPred` then `wp` is stronger than applying the transfer function then `toPred`.
Here `sp` refers to the strongest postcondition and `wp` refers to the weakest precondition (or perhaps wpIFRG).

# Function summary generation

TODO

# Loop invariant generation

In order for some procedures to verify it may be necessary for loop invariants to be annotated.
Take the following program for example.
```c
int main() {
    int x = 0;
    while (x < 100) {
        x++;
    }
    if (x != 100) {
        violate_security_requirements();
    }
}
```
Without a loop invariant this program may not verify as it may be impossible to prove that `x == 100` after the loop.
We use static analysis methods to generate loop invariants.
For example, the loop invariant `0 <= x <= 100` can be inferred using an interval analysis.

## Loop invariants in CFGs

BASIL programs are not represented in a denotational way but instead with a control flow graph.
This means that loops are not a fundamental construct and instead must be identified.
A loop of the form
```c
// ...
while (c) {
    // ...
}
// ...
```
will be translated into a CFG that looks like the following.
```
           Before loop
             ┌─────┐
             │ ... │
             └─────┘
                │
                v
            Loop header
           ┌───────────┐
    ┌──────│ assert j; │<───┐
    │      └───────────┘    │
    │            │          │
    v            v          │
After loop   Loop body      │
  ┌─────┐     ┌─────┐       │
  │ ... │     │ ... │───────┘
  └─────┘     └─────┘
```
We can encode a loop invariant as an assertion in the loop header.
Indeed doing so encodes the requirements that the loop invariant is ensured on entry and maintained by the loop, and also encodes that the loop invariant will hold at the start of the loop body and after the loop.

## Abstract interpretation for loop invariants

- see [src/main/scala/analysis/LoopInvariantGenerator.scala](https://github.com/UQ-PAC/BASIL/blob/main/src/main/scala/analysis/LoopInvariantGenerator.scala)

Any predicate encoding domain  can be used to generate loop invariants.
The result of an analysis on some procedure will be a mapping from blocks to lattice elements.
If the analysis is a forwards analysis, the predicate representation of the value at a block will be a property that always holds at the end of the block.
For backwards analyses, it will instead be a property that holds at the start of the block.

The `FullLoopInvariantGenerator` class performs a sequence of analyses and adds their results to the end or start of loop heads depending on if the analysis is a forwards or backwards analysis.
To add to this list, create either a `ForwardLoopInvariantGenerator` or `BackwardLoopInvariantGenerator` object and call its `genAndAddInvariants` method.
These singular generators take a domain as input, along with a solver to allow for using a solver which does not require either widening or narrowing.

To run the loop invariant generator, run basil with `--generate-loop-invariants` and without `--no-irreducibleloops`.
