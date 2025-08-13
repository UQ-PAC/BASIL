# Basil IR Code Transforms

##  Background: Static Analysis / Theory of Abstract Interpretation

Basil uses static analysis over the BASIL IR, to "lift" the program to a more abstract representation,
where we can make stronger assumptions about the code that make local reasoning more effective,
and hence verification easier.

This includes data-flow analyses based on the theory of Abstract Interpretation (AbsInt), as well
as constraint-based analyses.

- SPA:  [cs.au.dk/~amoeller/spa/spa.pdf](https://cs.au.dk/~amoeller/spa/spa.pdf)
  - Course textbook for CSSE4630, has a good introduction to lattice theory and dataflow analyses, the later
    chapters (Distributive and later) are less relevant.
- [Miné AbsInt tutorial](https://www-apr.lip6.fr/~mine/publi/article-mine-FTiPL17.pdf)
- [Lecture series on AbsInt (Youtube)](https://www.youtube.com/watch?v=FTcIE7uzehE&list=PLtjm-n_Ts-J-6EU1WfVIWLhl1BUUR-Sqm&index=27)


## DSA Form

- see [src/main/scala/ir/transforms/DynamicSingleAssignment.scala](https://github.com/UQ-PAC/BASIL/tree/main/src/main/scala/ir/transforms/DynamicSingleAssignment.scala)

We have a transform that establishes a dynamic single assignment form.
The DSA form makes it possible to perform a flow-insensitive analysis with some flow-sensitive precision,
with a single global abstract state, and one pass over the IR in control-flow order.

This form is established once, and should be maintained by subsequent transforms.

```
transforms.OnePassDSA().applyTransform(ctx.program)
```

This provides a property similar to SSA: that every use of a variable is defined by all syntactic definitions of the variable.
I.e. you never have the pattern below, where `x` has a different identity at different points in the program:
this only occurs if `x` is involved in a join, in which case all definitions are reached by the subsequent uses.

```c
x = 2
y = f(x)
x = 3
z = f(x)
```

i.e. the below is allowed

```c
if (c) {
  x = 2
} else {
  x = 3
}
y = f(x)

```

```c
x = 0
while (c) {
  x = x + 1
}
y = x

```

The analysis `transforms.rdDSAProperty(p: Procedure)` uses a relatively expensive
reaching definitions analysis to check that this property is satisfied, and is useful for
debugging.

## Dataflow analysis

See also : abstract interpretation

- see [src/main/scala/ir/transforms/AbsInt.scala](https://github.com/UQ-PAC/BASIL/tree/main/src/main/scala/ir/transforms/AbsInt.scala)

Most of the dataflow analyses in BASIL---those under `/src/main/scala/analysis`---use the
TIP frameork.
This framework uses a lot of generics in order to compose abstract domains and can be unweildy
to write new analyses for.

The new framework in `src/main/scala/transforms/Absint.scala` is both simpler to use and more performant.

A good example of using this framework is the simple live variables analysis. The abstract domain must
define a join, a bottom value, and a transfer function.

This similar analyses using a powerset can use the same root domain:

```scala
trait PowerSetDomain[T] extends AbstractDomain[Set[T]] {
  def bot = Set()
  def top = ???
  def join(a: Set[T], b: Set[T], pos: Block) = a.union(b)
}
```

Then live variables only has to define the transfer function for commands and jumps.

```scala
class IntraLiveVarsDomain extends PowerSetDomain[Variable] {
  // expected backwards

  def transfer(s: Set[Variable], a: Command): Set[Variable] = {
    a match {
      case a: Assign       => (s - a.lhs) ++ a.rhs.variables
      case m: MemoryAssign => s ++ m.index.variables ++ m.value.variables
      case a: Assume       => s ++ a.body.variables
      case a: Assert       => s ++ a.body.variables
      case i: IndirectCall => s + i.target
      case c: DirectCall   => (s -- c.outParams.map(_._2)) ++ c.actualParams.flatMap(_._2.variables)
      case g: GoTo         => s
      case r: Return       => s ++ r.outParams.flatMap(_._2.variables)
      case r: Unreachable  => s
    }
  }
}
```

We then create a solver using this domain, and pass it a procedure to solve to a fixed point.
The solver returns the abstract state at the beginning and end of every block.
This solver visits blocks in control flow order, which reduces the number of joins.
Passing the backwards flag makes it visit blocks (and statements) in the reverse control
flow order.

Ensure the order indexes are defined for every block using the function `transforms.applyRPO(program)`.

```scala
val liveVarsDom = IntraLiveVarsDomain()
val liveVarsSolver = worklistSolver(liveVarsDom)
val (beforeState, afterState) = liveVarsSolver.solveProc(procedure, backwards = true)
```

### IR AST Transforms

Expressions are immutable so can be freely modified without modifying the IR.
Statements are mutable however, so they can be changed by simply assigning to their members.

IR transforms can be applied using the `ir.cilvisitor.CILVisitor`.

This is just a tool for visiting the ast, and optionally modifying it.

At each node, e.g. a statement you can perform the actions:

- `DoChildren()` -- continue recursing
- `SkipChildren()` -- stop recursing
- `ChangeTo(e)` -- replace the node with the argument
- `ChangeDoChildrenPost(e, f: E => E)` -- first replace with the first argument, visit its children, then call function f on the result

Note that using `ChangeTo` on statements is often not desirable as it removes the old statement from the IR; invalidating references
to the statement that may be floating around in analysis results; instead the arguments to the statement may be changed by direct assignmet.

#### Variable substitution

The class `ir.transforms.Substitute` can be used to substitute variables into an expression.

This can be used as a twice-applied function, it first takes a function `Variable => Option[Expr]`
which defines the substitutions to perform. Note this matches the type signature of
`Map[Variable, Expr].get`, but can also be used to efficiently extract values from an `AbstractDomain[T]`.
The second application takes an expression and returns the expression with the values substituted.

```
val substitutions = Map((LocalVar("R3", ...) -> BitVecLiteral(0, 64)))
Substitute(substitutions.get)(expression)
```

#### Expression Evaluation / Simplification

The function `ir.eval.evaluateSimp` will perform concrete evaluation of an expression,
and returning `Some[Literal]` if it succeeds or `None` if it fails.

The function `ir.eval.simplifyExprFixpoint(e: Expr)` will simplify,
canonicalise, and perform partial evaluation on the expression, returning a new expression.

The function `ir.eval.cleanupSimplify(p: Procedure)` will further simplify known bits and redundant extends and
extracts within an expression.

##### Verification / Testing

Basil IR expressions can be converted so smt queries, for example to prove a basic tautology x = x we can write:

```scala
import tramslating.BasilIRToSMT2
val result = BasilIRToSMT2.proveExpr(BinaryExpr(BVEQ, R0, R0))
assert(result == Some(true))
```

This will negate the expression, translate it SMT2, invoke Z3, and check whether the result is "unsat".

We can also simply create the smt query with `BasilIRToSMT2.exprUnsat(expr, None, false)`

Note that statements and control-flow cannot be translated to SMT2 yet.
This system is currently used to validate the tranforms in the previous section.

### Simplification pass transform phases

Below is a rough outline of the transform passes enabled with using `--simplify`

#### Initial Cleanup

1. Find procedures that definitely exit on all paths, and replace calls to them in main with return statements
2. `transforms.addReturnBlocks`, lift return statements
    1. `IndirectCall R30; Unreachable` becomes `assert (R30 == R30_in); Return`
    2. `IndirectCall X; Unreachable` becomes `assert (R30 == R30_in); IndirectCall X; Return`
    3. `DirectCall X; Unreachable` becomes `assert (R30 == R30_in); DirctCall X; Return`
3. In each non-external procedure, a new return block is created and all `Return` statements replaced with a `GoTo` targeting this block
4. Remove implementations of external functions
5. Indirect calls are replaced by direct calls to a launchpad function with all registers as in and out parameters (and an additional register carrying the call target)
6. Replacing calls to other special functions (e.g. SVComp assert and assume, nondet) with abstract representations
7. Parameter form is established
    1. Interprocedural live-variables analysis using in/out param specifications for external functions
    2. Interprocedural modifies-set analysis using in/out param specifications for external functions
    3. All registers lifted to local variables, in/out formal param lists of procedures updated, in/out param lists of calls and returns updated
8. Simplification pass

#### Simplification Pass

1. Dynamic single assignment (`OnePassDSA`) form roughly comparable to [@braunSimpleEfficientConstruction2013a]. Establishes the invariant that \\(\forall v, use(v) \in proc\; \bullet (\forall def(v) \in proc \bullet reaching(use(v), def(v)))\\). Blocks are given the labels, filled: they have been given a local variable numbering, completed: they are filled and all their predecessors have been filled. Each block is visited once in reverse-post-order. An intraprocedural live-vars analysis is used to reduce the amount of copies added to outgoing edges. At each block:
    1.  All predecessors will be filled, compute their join by adding "back phi" blocks on the incoming edges which contain copies to make the renaming from each predecessor block coincide to a common renaming at the beginning of this block.
    2. Fill the current block and apply renaming starting from the join of the predecssors. Mark all predecessors complete that have been completed by filling this block.
    3. For all successors if they have already been filled add an intermediate block copying our outgoing renaming to the expected renaming of the successor by adding "forward phi" blocks.
2. Apply flow-insensitive intraprocedural copy-propagation, collecting variables that are used in condition flag calculations.
    1. Collect the set of replacements (`DSACopyProp`)
        - Restrict this to trivial copies (variables, and variables + constants), apply partial evaluation and simplifications in the abstract domain to keep the size of copies small.
        - Copy prop recursively in the abstract-domain and ensure clobbers invalidate all substituted dependencies
        - Reason about loads with array smashing on memory regions, load constants from read-only memory section
    2. Apply the variable substitution for the copyprop transform
    3. Perform second aggressive copy-prop pass with no limit on expression, but only collecting variables involved in flag calculations (`PropFlagCalculations`)
    4. Apply substitution of propagated flag calculation
3. Apply intra-block flow-sensitive copy-propagation (`BlockyProp`).
4. Apply term-rewriting simplifications (to a fixed point) to each statement (`AlgebraicSimplifications`)
5. Apply condition simplifications (to a fixed point) to each Assume and Assert condition (`AssumeConditionSimplifications`). This normalises towards a boolean representation (over bv1), and tries to simplify flag calculations and formulas of inequalities.
6. Remove redundant bitvector slicing operations using an approximate known-zero-bits and live-bits analysis. This tracks two numbers for each variable, reprsenting the most-significant accessed bit and the least-significant non-zero bit. It only considers zero-extends and bv_extracts in the transfer function. (`removeSlices`)
7. Apply term-rewriting simlifications to clean up locally redundant bitvector extend and extract operations. (`cleanupSimplify`)
8. Remove trivially dead variable assignments (`CleanupAssignments`)
9. Clean up block structure
    1. Coalesce straight-line blocks of assignments (often introduced by DSA form).
    2. Remove empty blocks.
    3. (repeated multiple times).
10. Re-apply intra-block flow-sensitive copy-propagation (benefits from block-coalescing).
11. Remove trivially dead variable assignments (`CleanupAssignments`)
12. Inline procedure return values which are either constant or only depend on the in-parameters by adding assignments after each call to the procedure.
13. Remove procedure in-parameters which are unused in the procedure (i.e. they were directly returned)

For (12) and (13) we can take the whole simplification pass to a fixed point to inline `n` calls deep (requires 12 iterations for cntlm). This proves R31 invariant across most procedures.

##### Invariant Checkers

Basic structural IR Invariants are enabled by default:

- `singleCallBlockEnd` : That calls only appear as the last statement in a block
- `cfgCorrect`: That the control-flow and call graphs embedded in the IR are correct and consistent (backwards and forwards)
- `blocksUniqeToEachProcedure`: That procedures don't accidentally share references to the same block (e.g. have gotos crossing procedures).
- `correctCalls`: That the procedure call structure is well-typed and actual parameter lists match formal parameter lists etc.

Further validation of the simplification pass is enabled by the `--validate-simplify` flag

- Ensure the DSA invariant is maintained using a reaching-definitions analysis.
    - This is uses reaching-defs implemented new analysis framework and can be differential-tested against the reaching-definitions implemented in TIP
- Expression rewrites performed by `AlgebraicSimplification`,
    `cleanupSimplify` and `AssumeConditionSimplification` are translated to SMT and verified

#### Output Pass

1. Remove unreachable procedures
2. Rename boogie reserved identifiers
3. Approximate stack identification if memoryregions are disabled
4. Boogie backend

## Known Bits Representation and Simplification

- see [src/main/scala/ir/transforms/SimplifyKnownBits.scala](https://github.com/UQ-PAC/BASIL/blob/main/src/main/scala/analysis/KnownBits.scala#L790)

### Representation ###

#### Bit Vectors ####
At different program states, there may be partially or fully unknown bitvector values in bitwise computations. For example, a variable `x` could be assigned a bitwise `OR` expression between two variables `y` and `z`, where `y` equals the binary value `00001111`, and `z` is not yet determined. To produce a value for `x` at the given program state, we will utilise a known bits representation, notably used in the Linux kernel and mentioned in [*Sound, Precise, and Fast Abstract Interpretation with Tristate Numbers*](https://people.cs.rutgers.edu/~sn349/papers/cgo-2022.pdf), to simultaneously track definite bits (`definite 0 / definite 1`) and unknown bits (`Top or T`).

The known bits representation can also be referred to as a ***TNum*** (tristate number) and consists of a value bitvector and a mask bitvector, stored within the TNumValue class as `TNumValue(value, mask)`. The value bitvector tracks the index of definite 1 bits and the mask bitvector tracks the index of unknown bits (T). This would mean that, if the value bit is 1 and the mask bit is 0, the TNum should represent a definite 1 at that specific index. On the other hand, if the value bit is 0 and the mask bit is 1, the TNum should represent T (Top) at the index. This would also mean that the index of definite 0 bits can be inferred when the value and mask bits at a certain index are both 0. Furthermore, the value and mask bits at the same index can never be 1 simultaneously as this is not a valid state.

With this system, we can represent the variable `y` in our previous example as `y = TNumValue(00001111, 00000000)` and the variable `z` as `z = TNumValue(00000000, 11111111)`, which are equivalent to `00001111` and `TTTTTTTT`, respectively.

#### Booleans ####
The section below will outline all supported TNum operations, which also include comparison operators that return a boolean type. Thus, a TNum boolean type will also be required to maintain consistency in the TNum abstract domain. The TNum boolean class will take the form of `TNumBool(boolean: Int)`, where its boolean value is expressed as either 1 or 0, indicating true or false respectively.

### Simplification ###
The simplification of TNumValue expressions is also supported within the TNumValue class. Taking our example earlier and substituting in TNums `y` and `z`, we have:

```
x = TNumValue(00001111, 00000000) | TNumValue(00000000, 11111111)
```

or equivalently,

```
x = 00001111 | TTTTTTTT
```

All supported operators can be found under the TNumValue and TNumBool classes as class methods. These methods ensure that all uncertainty are propagated correctly. For example, the bitwise `OR` operation method, `TOR`, which can be found under the TNumValue class, follows the logic:

```scala
def TOR(that: TNumValue): TNumValue = {
  val v = this.value | that.value
  val mu = this.mask | that.mask
  TNumValue(v, (mu & ~v));
}
```
The return value of this method is equivalent to computing the equation above, that is, simplifying both TNumValues `y` and `z`, and storing the result in a new TNumValue `x`. The new value bitvector `v` of `x` can be obtained by applying the bitwise `OR` operation on both value bitvectors of `y` and `z`:

```scala
val v = 00001111 | 00000000 = 00001111
```

The new mask bitvector `x` can be obtained by first applying the bitwise `OR` operation on both mask bitvectors of `y` and `z` (`mu`), and then applying the bitwise `AND` operation on both `mu` and `NOT` `v`:

```scala
val mu = 00000000 | 11111111 = 11111111
mu & ~v = 11111111 & ~00001111 = 11111111 & 11110000 = 11110000
```

We finally obtain:

```
x = TNumValue(00001111, 11110000) = TTTT1111
```

This result aligns with the expected TNum logic since `0 | T` must be `T` given that T could be either 0 or 1, and `1 | T` can be immediately simplified to `1` since the expression will always evaluate to 1 regardless of the value of T.

#### Usage / Purpose ####

The known bits simplification technique has been proven to reduce computational overhead by eliminating redundant instructions. Suppose we have the following condition:

```scala
if (x[0] == 1) {
  ...
}
```

By simplifying:

```
x = 00001111 | TTTTTTTT
```

into

```
x = TTTT1111
```

we can deduce beforehand that index 0 of *x*, `x[0]`, will always be 1, and the condition will be satisfied.

#### Simplification Testing ####

- see [scripts/tnum_test_z3.py](https://github.com/UQ-PAC/BASIL/tree/main/scripts/tnum_test_z3.py)

Within the provided Z3 Python script, the supported operator methods have been translated to Python/Z3-equivalent methods that utilise the same logic but with varying syntax. The Z3 solver is then able to deduce whether the TNumValue returned after the operation adequately represents the full range of possible results and correctly propagates any uncertainty. The logic of each method is deemed "correct" if it satisfies three different criteria: ***soundness***, ***wellformedness***, and ***precision***.

***Soundness*** refers to whether the abstract TNumValue contains all concrete results, that is, the TNumValue must be able to represent every possible correct concrete value. For example, if an operation is expected to output any of the following binary values: `001`, `010`, `011`, then any logic returning `0TT`, or `TNumValue(000, 011)`, would be a sound solution.

***Wellformedness*** refers to the condition that a TNumValue does not violate the defined structure of a typical TNumValue. Specifically, a TNumValue is only wellformed if it does not have a value bit and a mask bit set to 1 at the **same** index, as this is not a valid state for a TNumValue. For example, `TNumValue(100, 100)` is not wellformed.

***Precision*** refers to how precise the TNumValue is. In other words, a precise TNumValue should not over-approximate the range of possible results, unnecessarily represent values that should not appear, or neglect bits that are already known. For example, if an operation is expected to output any of the following binary values: `001`, `010`, `011`, then any logic returning `0TT`, or `TNumValue(000, 011)`, would be more precise than logic that returns `TTT`, or `TNum(000, 111)`. This is because we can clearly identify the MSB as 0, and therefore, the definite 0 bit should be retained rather than approximating it as either 0 or 1.

### Transfer Function ###
The known bits analysis mainly focuses on simplifying local assignment commands or statements (`LocalAssign`), and handles `MemoryLoad` statements by defaulting to a fully unknown TNumValue. This can be seen within the `transfer(s: Map[Variable, TNum], b: Command): Map[Variable, TNum]` method that converts each `Command` expression into a TNum, maps the assigned variable to the resultant TNum, and returns the variable mapping of the current program state. To simplify the transfer method, the `evaluateExprToTNum(s: Map[Variable, TNum], expr: Expr): TNum` method has been implemented to allow for recursive evaluation of both nested and non-nested expressions that appear within `LocalAssign` statements.


## Analysis framework design reference

- [Scala tutorial for a toy language](https://web.archive.org/web/20250317112721/https://continuation.passing.style/blog/writing-abstract-interpreter-in-scala.html)
  - This is substantially different to what we ended up implementing, but is a useful reference as related work

