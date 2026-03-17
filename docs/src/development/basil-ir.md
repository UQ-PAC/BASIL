# BASIL IR

BASIL IR is the intermediate representation used during static analysis.
This is on contrast to Boogie IR which is used for specification annotation, and output to textual boogie syntax that can be run through the Boogie verifier.

The textual representation of basil IR has a grammar describing it [here](https://uq-pac.github.io/BASIL/docs/basil-il/BasilIR.html).
Names used internally differ slightly.

The IR has three levels:

- Procedure / Program / Block, defined in [Program.scala](https://github.com/UQ-PAC/BASIL/blob/main/src/main/scala/ir/Program.scala)
  - Encodes the program control-flow
- Commands : Jumps and Statements, defined in [Statement.scala](https://github.com/UQ-PAC/BASIL/blob/main/src/main/scala/ir/Statement.scala)
  - Encodes atomic program steps
- Expressions, defined in [Expr.scala](https://github.com/UQ-PAC/BASIL/blob/main/src/main/scala/ir/Expr.scala)
  - Encodes atomic and pure (sometimes partial) computations. These are components of statements.

The IR has a completely standard simple type system that is enforced at construction.

### Structure

Describes the high-level structure of the in-memory IR ADT

```math
\begin{align*}
Program ::=&~ Procedure* \\
Procedure ::=&~ (name: ProcID) (entryBlock: Block) (returnBlock: Block) (blocks: Block*) \\
               &~ \text{Where }entryBlock, returnBlock \in blocks \\
Block_1 ::=&~ BlockID \; Statement*\; Call? \; Jump \; \\
Block_2 ::=&~ BlockID \; (Statement | Call)*\; Jump \; \\
\\
&~ Block = Block_1 \text{ is a structural invariant that holds during all the early analysis/transform stages}
\\
Statement ::=&~ MemoryAssign ~|~ LocalAssign ~|~ Assume ~|~ Assert ~|~ NOP \\
ProcID ::=&~ String \\
BlockID ::=&~ String \\
\\
Jump ::=&~ GoTo ~|~ Unreachable ~|~ Return \\
GoTo ::=&~ \text{goto } BlockID* \\
Return::=&! \text{return } (outparams)
Call ::=&~ DirectCall ~|~ IndirectCall  \\
DirectCall ::=&~ (outparams) := \text{ call } ProcID \; (inparams) \\
IndirectCall ::=&~ \text{call } Expr \\
\\
          &~ loads(e: Expr) = \{x |  x:MemoryLoad, x \in e \} \\
\\
MemoryAssign ::=&~ MemoryAssign (mem: Memory) (addr: Expr) (val: Expr) (Endian) (size: Int) \\
          &\text {Such that } loads(addr) = loads(val) = \emptyset \\
\\
LocalAssign ::=&~ Variable := Expr \\
Assume ::=&~ \text{assume } body:Expr\\
          &\text {Such that } loads(body) = \emptyset \\
Assert ::=&~ \text{assert } body:Expr\\
          &\text {Such that } loads(body) =  \emptyset \\
\\
Expr ::=&~ MemoryLoadExpr ~|~ Variable ~|~ Literal ~|~ Extract ~|~ Repeat \\
          &~ ~|~ ZeroExtend ~|~ SignExtend ~|~ UnaryExpr ~|~ BinaryExpr \\
Variable ::=&~ Global ~|~ LocalVar \\
MemoryLoadExpr ::=&~  MemoryLoad (mem:Memory)  (addr: Expr)  (Endian) (size: Int) \\
          &\text {Such that } loads(addr) = \emptyset \\
Memory ::=&~ Stack ~|~ Mem \\
Endian ::=&~ BigEndian ~|~ LittleEndian \\
\end{align*}
```

### Jumps

- The `GoTo` jump is a multi-target jump reprsenting non-deterministic choice between its targets.
  Conditional structures are represented by these with a guard (an assume statement) beginning each target.
- The `Unreachable` jump is used to signify the absence of successors, it has the semantics of `assume false`.
- The `Return` jump passes control to the calling function, often this is over-approximated to all functions which call the statement's parent procedure.

### Calls

- A `DirectCall` calls a procedure and returns to the next command in the block
-  An `IndirectCall` is a dynamic jump, to either a procedure or a block based on the address of the argument

## Translation Phases

We have invariant checkers to validate the structure of the IR's bidirectional CFG is correct, see `src/main/scala/ir/invariant`. This includes:

- blocks belong to exactly one procedure: `invariant/BlocksUniqueToProcedure.scala`
- forwards block CFG links match backwards block CFG links: `invariant/CFGCorrect.scala`

#### IR With Returns

- Immediately after loading the IR return statements may appear in any block, or may be represented by indirect calls.
  The transform pass below replaces all calls to the link register (R30) with return statements.
  In the future, more proof is required to implement this soundly.

```
cilvisitor.visit_prog(transforms.ReplaceReturns(), ctx.program)
transforms.addReturnBlocks(ctx.program, true) // add return to all blocks because IDE solver expects it
cilvisitor.visit_prog(transforms.ConvertSingleReturn(), ctx.program)
```

This ensures that all returning, non-stub procedures have exactly one return statement residing in their `returnBlock`.

#### Calls appear only as the last statement in a block

- Checked by `invariant/SingleCallBlockEnd.scala`
- The structure of the IR allows a call may appear anywhere in the block but for all the analysis passes we hold the invariant that it
  only appears as the last statement. This is checked with the function `singleCallBlockEnd(p: Program)`.
  And it means for any call statement `c` we may `assert(c.parent.statements.lastOption.contains(c))`.

## IR With Parameters

The higher level IR containing parameters is established by `ir.transforms.liftProcedureCallAbstraction(ctx)`.
This makes registers local variables, which are passed into procedures through prameters, and then returned from
procedures. Calls to these procedure must provide as input parameters the local variables corresponding to the
values passed, and assign the output parameters to local variables also. Note now we must consider indirect calls
as possibly assigning to everything, even though this is not explicitly represented syntactically.

- Actual parameters to calls and returns match formal parameters is checked by `invariant/CorrectCallParameters.scala`

## Interaction with BASIL IR

### Constructing Programs in Code

The 'DSL' is a set of convenience functions for constructing correct IR programs in Scala source files.
This provides a simple way to construct IR programs for use in unit tests.
Its source code can be [found here](https://github.com/UQ-PAC/BASIL/tree/main/src/main/scala/ir/dsl/DSL.scala).

An example can be seen below:

```scala
var program: Program = prog(
  proc("main",
    block("first_call",
      Assign(R0, bv64(1), None)
      Assign(R1, bv64(1), None)
      directCall("callee1"),
      goto("second_call"))
    ),
    block("second_call",
      directCall("callee2"),
      goto("returnBlock")
    ),
    block("returnBlock",
      ret
    )
  ),
  // ... other procedures
)
```

As we can see, the syntax is

```
program     ::= prog ( procedure+ )
procedure   ::= proc (procname, block+)
block       ::= block(blocklabel, statement+, jump)
statement   ::= <BASIL IR Statement>
jump        ::= goto_s | ret | unreachable
call_s      ::= directCall (procedurename, None | Some(blocklabel))  // target, fallthrough
goto_s      ::= goto(blocklabel+)                              // targets
procname    ::= String
blocklabel  ::= String
```

If a block or procedure name is referenced in a target position, but a block or procedure is not defined with that
label, the dsl constructor will likely throw a match error.

Some additional constants are defined for convenience, Eg. `R0 = Register(R0, 64)`, see [the source file](https://github.com/UQ-PAC/BASIL/tree/main/src/main/scala/ir/dsl/DSL.scala) for the full list.


### Pretty printing

The IR can be printed with the overloaded function below, which can take a procedure, block, or statement and returns a string.

```scala
translating.BasilIRPrettyPrinter()(b)
```

It is also possible to dump a `dot/graphviz` digraph containing just the blocks in the program
using the functions:

```scala
ir.dotBlockGraph(prog: Program) : String
ir.dotBlockGraph(proc: Procedure) : String
```

### Static Analysis / Abstract Interpretation / IR Rewriting and modification

- See [transforms.md](transforms.md)
- For static analysis, the IL-CFG-Iterator is the current well-supported way to iterate the IR.
  This currently uses the TIP framework, so you do not need to interact with the IR visitor directly.
  See [ConstantPropagation.scala](https://github.com/UQ-PAC/BASIL/blob/main/src/main/scala/analysis/ConstantPropagation.scala) for an example on its useage.
- This visits all procedures, blocks and statements in the IR program.
