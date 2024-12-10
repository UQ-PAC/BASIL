## New Anslysis & Transforms

This page describes the framework for static analysis used by the `--simplify` pass. 

## DSA Form

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

Most of the dataflow analyses in BASIL---those under `/src/main/scala/analysis`---use the 
TIP frameork.
This framework uses a lot of generics in order to compose abstract domains and can be unweildy
to write new analyses for.

The new framework in `src/main/scala/transforms/Absint.scala` is both simpler to use and more performant.

A good example of usning this framework is the simple live variables analysis. The abstract domain must 
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
