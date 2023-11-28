CFG Iterator Implementation
===========================

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
    - The traversal order is defined by the order of terms in the language with a depth-first traversal of sub-terms.
2. Its control flow graph; this is part of the language's semantics, and is inferred from the Jump and Call statements.
    - This is traversed using the control flow iterator, or by constructing the separate Tip-style CFG and traversing that.
      From here on we describe the 'control-flow iterator'.
    - The traversal order is defined by the `Dependency` structure and `Worklist` solvers and the predecessor/successor
      relation between pairs of nodes

We need to derive the predecessor/successor relation on CFG nodes IL .

1. CFG positions are defined as 
    - The entry to a procedure
    - An exit from a procedure
    - The beginning of a block within a procedure
    - A statement command within a block
    - A jump or call command within a block

For example we define the language as statements for horn clauses. (`A :- B` means B produces A, with `,` indicating 
conjunction and `;` indicating disjunction)

First we have basic blocks belonging to a procedure. 

    Procedure(id)
    Block(id, procedure) 

A list of sequential statements belonging to a block

    Statement(id, block, index)

A list of jumps (either Calls or GoTos) belonging to a block, which occur after the statements. GoTos form the 
intra-procedural edges, and Calls form the inter-procedural edges. 

    GoTo(id, block, index, destinationBlock) 
    Call(id, block, index, destinationProcedure) 
    Jump(id, block) :- GoTo(id, block, _) ; Call(id, block, _)

Statements and Jumps are both considered commands. All IL terms, commands, blocks, and procedures, have a unique
identifier. All of the above are considered IL terms.

    Command(id) :- Statement(id, _, _) ; Jump(id, _)
    ILTerm(id) :- Procedure(id); Block(id, _); Command(id) 

The CFG extends this language with the following nodes:

    ProcedureExit(id, fromProcedure, fromJump)
    CallReturn(id, fromCall)

    CFGNode(id) :- ProcedureExit(id,_,_) ; CallReturn(id,_) ; ILTerm(id)

The predecessor/successor relates CFGNodes to CFGNodes, and is simply defined in terms of the nodes 

    pred(i, j) :- succ(j, i)

    succ(block, statement) :- Statement(statement, block, 0)
    succ(statement1, statement2) :- Statement(statement1, block, i), Statement(statement2, block, i + 1)
    succ(statement, goto) :- Statement(block, _last), Jump(block, goto), _last = max i forall Statement(block, i)

    succ(goto, targetBlock) :- GoTo(goto, _, _, targetBlock) 

    // We always insert nodes for calls to return to
    CallReturn(i, call) :- Call(call, _, _, _)
    succ(call, callreturn) :- CallReturn(callreturn, call), Procedure(call)

    // a 'return' from the procedure is an indirect call to register R30
    succ(call, exit) :- Call(call, block, _, "R30"), ProcedureExit(exit, procedure, call), Block(block, procedure)

For an inter-procedural CFG we also have:

    succ(call, targetProcedure) :- Call(call, _, _, targetProcedure) 
    succ(exit, returnNode) :- ProcedureExit(exit, procedure, call), CallReturn(returnNode, call)

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

To maintain this superimposed graph it is necessary to make the actual call lists private, and only allow 
modification of through interfaces which maintain the graph.  

Maintenance of the graph is the responsibility of the Block class: adding or removing jumps must ensure the edge 
    references are maintained.

Jumps:
- Must implement an interface to allow adding or removing edge references (references to themself) to and from their 
  target 

Blocks and Procedures:
- Implement an interface for adding and removing edge references 

Furthermore;
- Reparenting Blocks and Commands in the IL must preserve the parent field, this is not really implemented yet