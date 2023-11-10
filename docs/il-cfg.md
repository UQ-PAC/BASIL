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

For example we define the language as horn clauses;

First we have basic blocks belonging to a procedure. 

    Procedure(id)
    Block(id, procedure) 

A list of sequential statements belonging to a block

    Statement(id, block, index)

A list of jumps (either Calls or GoTos) belonging to a block, which occur after the statements. GoTos form the 
intraprocedural edges, and Calls form the interprocedural edges. 

    GoTo(id, block, index, destinationBlock) 
    Call(id, block, index, destination) 

The CFG extends this language with the following nodes:

    ProcedureExit(id, procedure, fromJump)
    CallReturn(id, fromCall)

A statement and jump is both considered a command. All IL terms, commands, blocks, and procedures, have a unique 
identifier. 

    Jump(id, block) :- GoTo(id, block, _) 
    Jump(id, block) :- Call(id, block, _) 
    Command(id) :- Statement(id, _, _)
    Command(id) :- Jump(id, _)

The predecessor/successor relation is defined as a function of the CFG node.

    pred(i, j) :- succ(j, i)

    succ(block, statement) :- Statement(statement, block, 0)
    succ(statement1, statement2) :- Statement(statement1, block, i), Statement(statement2, block, i + 1)
    succ(statement, goto) :- Statement(block, _last), Jump(block, goto), _last = max i forall Statement(block, i)

    succ(goto, block) :- GoTo(goto, _, _, block) 

    // We always insert nodes for calls to return to
    CallReturn(i, call) :- Call(call, _, _, _)
    succ(call, callreturn) :- CallReturn(callreturn, call), Procedure(call)

    // a 'return' from the procedure is an indirect call to register R30
    succ(call, exit) :- Call(call, block, _, "R30"), ProcedureExit(exit, procedure, call), Block(block, procedure)

For an interprocedural CFG we also have:

    succ(call, procedure) :- Call(call, _, _, procedure) 
    succ(exit, returnNode) :- ProcedureExit(exit, procedure, call), CallReturn(returnNode, call)

Implementation
--------------

We want it to be possible to define `succ(term, _)` and `pred(term, _)` for any given term in the IL in `O(1)`. 
Successors are easily derived but predecessors are not stored with their successors. Furthermore `ProcedureExit`, 
and `CallReturn` are not inherently present in the IL. 

In code we have a set of Calls, and Gotos present in the IL; these form the edges. Then all vertices in the CFG 
store a list of references to their set of incoming and outgoing edges. Specifically this means

    Statement:
        - reference to parent block
        - procedure to find the next or previous statement in the block

    Block
        - reference to parent procedure
        - list of incoming GoTos
        - list of Jumps 
            - Outgoing Calls
            - Outgoing GoTos
        
    Procedure
        - list of incoming Calls
        - subroutine to compute the set of all outgoing calls in all contained blocks

To maintain this superimposed graph it is neccessary to make the actual call lists private, and only allow 
modification of through interfaces which maintain the graph.  

Maintenence of the graph is the responsibility of the Block class: adding or removing jumps must ensure the edge 
    references are maintained.

Jumps:
- Must implement an interface to allow adding or removing edge references (references to themself) to and from their 
  target 

Blocks and Procedures:
- Implement an interface for adding and removing edge references 


Furthermore;
- Reparenting Blocks and Commands in the IL must preserve the parent field, this is not really implemented yet

