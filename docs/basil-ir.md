# BASIL IR

BASIL IR is the intermediate representation used during static analysis. 
This is on contrast to Boogie IR which is used for specification annotation, and output to textual boogie syntax that can be run through the Boogie verifier. 

The grammar is described below, note that the IR is a data-structure, without a concrete textual representation so the below grammar only represents the structure. 
We omit the full description of the expression language because it is relatively standard.  

The IR has a completely standard simple type system that is enforced at construction.

```math
\begin{align*}
Program ::=&~ Procedure* \\
Procedure ::=&~ (name: ProcID) (entryBlock: Block) (returnBlock: Block) (blocks: Block*) \\
               &~ \text{Where }entryBlock, returnBlock \in blocks \\
Block ::=&~ BlockID \; (Statement*)\; Jump \; (fallthrough: (GoTo | None))\\
         &~ \text{Where $fallthough$ may be $GoTo$ IF $Jump$ is $Call$} \\
Statement ::=&~ MemoryAssign ~|~ LocalAssign ~|~ Assume ~|~ Assert ~|~ NOP \\
ProcID ::=&~ String \\
BlockID ::=&~ String \\
\\
Jump ::=&~ Call ~|~ GoTo \\
GoTo ::=&~ \text{goto } BlockID* \\
Call ::=&~ DirectCall ~|~ IndirectCall  \\
DirectCall ::=&~ \text{call } ProcID \\
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

## Interaction with BASIL IR

### Constructing Programs in Code

The 'DSL' is a set of convenience functions for constructing correct IR programs in Scala source files. 
This provides a simple way to construct IR programs for use in unit tests.
Its source code can be [found here](../src/main/scala/ir/dsl/DSL.scala).

An example can be seen below:

```scala 
var program: Program = prog(
  proc("main",
    block("first_call",
      Assign(R0, bv64(1), None)
      Assign(R1, bv64(1), None)
      directCall("callee1", Some("second_call"))
    ),
    block("second_call",
      directCall("callee2", Some("returnBlock"))
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
jump        ::= call_s | goto_s | ret
call_s      ::= directCall (procedurename, None | Some(blocklabel))  // target, fallthrough 
goto_s      ::= goto(blocklabel+)                              // targets
procname    ::= String
blocklabel  ::= String
```

If a block or procedure name is referenced in a target position, but a block or procedure is not defined with that 
label, the dsl constructor will likely throw a match error. 

Some additional constants are defined for convenience, Eg. `R0 = Register(R0, 64)`, see [the source file](../src/main/scala/ir/dsl/DSL.scala) for the full list.

### Static Analysis / Abstract Interpretation

- For static analysis the Il-CFG-Iterator is the current well-supported way to iterate the IR.
  This currently uses the TIP framework, so you do not need to interact with the IR visitor directly. 
  See [BasicIRConstProp.scala](../src/main/scala/analysis/BasicIRConstProp.scala) for an example on its useage.
- This visits all procedures, blocks and statements in the IR program.

### Modifying and Visiting the IR with Visitor Pattern

[src/main/scala/ir/Visitor.scala](../src/main/scala/ir/Visitor.scala) defines visitors which can be used
for extracting specific features from an IR program. This is useful if you want to modify all instances of a specific 
IR construct.
  
### CFG 

The cfg is a control-flow graph constructed from the IR, it wraps each statement in a `Node`.

