CFG Iterator Implementation
===========================

The IR is the internal data structure representing the program and its control flow. 

The language is described roughly below. The full description of expressions is missing.  

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
          &~ stores(e: Expr) = \{x |  x:MemoryStore, x \in e \} \\
\\
MemoryAssign ::=&~ Memory := MemoryStore \\
LocalAssign ::=&~ Variable := Expr \\
Assume ::=&~ \text{assume } body:Expr\\
          &\text {Such that } loads(body) = stores(body) = \emptyset \\
Assert ::=&~ \text{assert } body:Expr\\
          &\text {Such that } loads(body) = stores(body) = \emptyset \\
\\
Expr ::=&~ MemoryStore ~|~ MemoryLoad ~|~ Variable ~|~ Literal ~|~ Extract ~|~ Repeat \\
          &~ ~|~ ZeroExtend ~|~ SignExtend ~|~ UnaryExpr ~|~ BinaryExpr \\
Variable ::=&~ Global ~|~ LocalVar \\
MemoryStore ::=&~  (mem:Memory) (addr: Expr) (val: Expr) (Endian) (size: Int) \\
          &\text {Such that } loads(addr) = loads(val) = stores(addr) = stores(val) = \emptyset \\
MemoryLoad ::=&~  (mem:Memory)  (addr: Expr)  (Endian) (size: Int) \\
          &\text {Such that } loads(addr) = stores(addr) = stores(val) = \emptyset \\
Memory ::=&~ Stack ~|~ Mem \\
Endian ::=&~ BigEndian ~|~ LittleEndian \\
\end{align*}
```



This file explains the in-place CFG representation on top of the IL.

Motivations
-----------

We want a unified IL and CFG representation to avoid the problem of keeping two datastructures in sync, 
and to essentially avoid the problem of defining the correspondence between the static analysis state domain, and 
the IL in order to apply a transformation to the IL using the CFG results.  

It also reduces the number of places refactors need to be applied, and reduces memory overhead for static analyses 
(hopefully). 


Interpreting the CFG from the IL
--------------------------------

The IL has two structural interpretations:

1. Its syntax tree; expressions have sub expressions and so on.
    - This can be traversed using Visitors
    - It can also be traversed down by accessing class fields, and upward using the Parent trait
    - The traversal order is defined by the order of terms in the language with a depth-first traversal of sub-terms.
2. Its control flow graph; this is part of the language's semantics, and is inferred from the Jump and Call statements.
    - This is traversed using the control flow iterator, or by constructing the separate Tip-style CFG and traversing that.
      From here on we describe the 'control-flow iterator'.
    - The traversal order is defined by the `Dependency` structure and `Worklist` solvers and the predecessor/successor
      relation between pairs of nodes

We need to derive the predecessor/successor relation on CFG nodes IL .

1. CFG positions are defined as 
    - The entry to a procedure
    - The single return point from a procedure
    - The block and jump statement that return from the procedure 
    - The beginning of a block within a procedure
    - A statement command within a block
    - A jump or call command within a block

For example we define the language as statements for horn clauses. (`A :- B` means B produces A, with `,` indicating 
conjunction and `;` indicating disjunction)

First we have basic blocks belonging to a procedure. 

    Procedure(id)
    Block(id, procedure) 
    EntryBlock(block_id, procedure)
    ReturnBlock(block_id, procedure) 
    Block(id, procedure) :- EntryBlock(id, procedure); ReturnBlock(id, procedure); AfterCallBlock(id, call)

A list of sequential statements belonging to a block

    Statement(id, block, index)

A list of jumps (either Calls or GoTos) belonging to a block, which occur after the statements. GoTos form the 
intra-procedural edges, and Calls form the inter-procedural edges. 

    GoTo(id, block, destinationBlock)  // multiple destinations
    Call(id, block, destinationProcedure, returnBlock), count {Call(id, block, _, _)} == 1 
    Jump(id, block) :- GoTo(id, block, _) ; Call(id, block, _, _)

Statements and Jumps are both considered commands. All IL terms, commands, blocks, and procedures, have a unique
identifier. All of the above are considered IL terms.

    Command(id) :- Statement(id, _, _) ; Jump(id, _) 
    ILTerm(id) :- Procedure(id); Block(id, _); Command(id) 

The predecessor/successor relates ILTerms to ILTerms, and is simply defined in terms of the nodes 

    pred(i, j) :- succ(j, i)

    succ(block, statement) :- Statement(statement, block, 0)
    succ(statement1, statement2) :- Statement(statement1, block, i), Statement(statement2, block, i + 1)
    succ(statement, goto) :- Statement(block, _last), Jump(block, goto), _last = max i forall Statement(block, i)

    succ(goto, targetBlock) :- GoTo(goto, _, _, targetBlock) 

    succ(call, aftercall_block) :- Call(call, block, dest_procedure, aftercall_block), AfterCallBlock(aftercall_block, call)

For an inter-procedural CFG we also have:

    succ(return_block, aftercall_block) :- ReturnBlock(return_block, proc), AfterCallBlock(call,), Call(call, _,_, proc),  
            Procedure(proc)
    succ(call, targetProcedure) :- Call(call, _, _, targetProcedure), Procedure(targetProcedure) 

An inter-procedural solver is expected to keep track of call sites which return statements jump back to.

So a sequential application of `succ` might look like

    ProcedureA -> {Block0} -> {Statement1} -> {Statement2} -> {Jump0, Jump1} ->  {Block1} | {Block2} -> ...

Implementation
--------------

We want it to be possible to define `succ(term, _)` and `pred(term, _)` for any given term in the IL in `O(1)`. 
Successors are easily derived but predecessors are not stored with their successors. Furthermore `ProcedureExit`, 
and `CallReturn` are not inherently present in the IL. 

In code we have a set of Calls, and Gotos present in the IL: these define the edges from themselves to their target. 

Then all vertices in the CFG---that is all Commands, Blocks, and Procedures in the IL---store a list of references to 
their set of incoming and outgoing edges. In a sense the 'id's in the formulation above  become the JVM object IDs.

For Blocks and Procedures this means a `Set` of call statements. For Commands this means they are 
stored in their block in an intrusive linked list. 

Specifically this means we store

    Command:
        - reference to parent block
        - procedure to find the next or previous statement in the block
        - IntrusiveListElement trait inserts a next() and previous() method forming the linked list

    Block
        - reference to parent procedure
        - list of incoming GoTos
        - list of Jumps including
            - Outgoing Calls
            - Outgoing GoTos
        
    Procedure
        - list of incoming Calls
        - subroutine to compute the set of all outgoing calls in all contained blocks

This means the IL contains:
- Forward graph edges in the forms of calls and gotos
- Forward syntax tree edges in the form of classes containing their children as fields
- Backwards graph edges in the form of lists of incoming jumps and calls
    - Procedure has list of incoming calls
    - Block has list of incoming gotos
- Backwards syntax tree edges in the form of a parent field
    - Implementation of the `HasParent` trait.

To maintain the backwards edges it is necessary to make the actual data structures private, and only allow
modification through interfaces which maintain the graph/tree.

Jumps:
- Must implement an interface to allow adding or removing edge references (references to themself) to and from their
  target

Blocks and Procedures:
- Implement an interface for adding and removing edge references

Furthermore;
- Reparenting Blocks and Commands in the IL must preserve the parent field, this is not really implemented yet

`HasParent` Relation
------------------
 
The parent invariant means that any node that is connected to the IR/CFG: i.e. nodes reachable from a Program, Procedure, 
or Block, control-flow, etc. has a parent defined. On removal from the program `deParent()` is called, and on adding 
`setParent(newparent)` is called. These in turn call `linkParent` and `unlinkParent`. 

Implementations of these methods are responsible for maintaining the other invariants of the IR. For example, the 
invariant that the target of every goto contains a reference to the goto in its `incomingJumps` set.
