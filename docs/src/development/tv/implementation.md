# Translation Validation Implementation

At the highest level the translation validation takes two programs, an invariant linking them
and produces, constructs product program describing the simultaneous execution of the two programs, 
and verifies this program satisfies the invariant.

We initially describe the translation pipeline and structures used throughout this process.

## Phases

### Cut-Transition System

- `TransitionSystem.scala`
- Transforms a Basil IR program to an equivalent acyclic Basil IR program

A transition system describes one acyclic aggregate program step. This step breaks a single 
Basil IR program into a program which represents a single acyclic step at a time.
This effectively fans out every loop in the program into a single loop which is equivalent
to the original program.

#### Cut transform:

  1. Create a new entry and exit
  2. Use program entry as a cut link it to the new entry and guard with a specific PC value `ENTRY`
  3. Use program exit as a cut link it to the new exit and set with a specific PC value `RETURN`
  2. Identify each loop header as a cut, set `PC := Loop$i` and redirect through exit, add edge 
     from entry to header guarded by a pc value `Loop$i`

### Monadic Local Side-Effect Form

- `SSADAG.scala` and `Ackermann.scala`

This translates the Basil IR program to a program containing three statement types:

1. (Simultaneous) assignment `(a, b, c) := (d, e, f)` (Scala type `SimulAssign`)
2. Side effect calls : `(TRACE, a, b, c) := EffName (TRACE, d, e, f)` (Scala type `SideEffectStatement`). 
  This is an uninterpreted function call with multiple/tuple return.
3. Assumes / Guards

Note the `TRACE` boolean-typed variable here which represents the state passed through the program.
This is where the "monadic" terminology comes from. A boolean type is sufficient here as it only 
needs to represent the truth value of the equivalence between the source and target trace.

Think of this `TRACE` as an oracle represnting the entire universe, i.e. we assume
the precondition `TRACE_source == TRACE_target`. This is assuming the programs execute at the
same time in the same universe state; thus it captures external IO, assuming
both programs will always receive identical external inputs if they are invariantly 
in the same state.

A frame analysis is used to identify the interprocedural effects of calls. This transform
pulls these side effects (memory access, global variable access) into the parameter list
of the side-effect statement.

### SSA Form

This performs a naive SSA transform (not considering loops) on the Monadic program.

It introduces reachability predicates (`${blockName}_done`) for the end of every block. 
This predicate is the conjunction of 

1. the disjunction of the reachability of its predcessors and
2. conjunction of all assume statements in the block.

Note the phi nodes have a slightly odd structure so they fit in the existing Basil IR.
In the below code, the assume at the start of block `l3` represents the phi node
joining `l1` and `l2`.


```c
block l1  [
  r0_0 := 1; // was r0 := 1
  goto  l3;
];
block l2  [
  r0_1 := 2; // was r0 := 2
  goto  l3;
];
block l3  [
  assume (l1_done ==> r0_3 == r0_0 && l2_done ==> r0_3 == r0_2);
  ret; 
];
```

This transform returns a function which renames an un-indexed expression 
to one in terms of the ssa-indexed variables defined at a given block.

### Ackermannisation

- This is an invariant inference pass perfomed on the SSA-form program

This is a transform which soundly performs the reasoning about the correspondence of
side effects in the product program ahead of verification-time. 

At a high level, assume we have side-effect statements in the source and target program below:

```
// source program:
(source_TRACE_1, source_R0_1) := source_Load_mem_64 (source_TRACE, source_R1);
// target program:
(target_TRACE_1, target_R0_1) := target_Load_mem_64 (target_TRACE, target_R1);
```

Analagous to the congruence rule of uninterpreted functions we have the axiom:

```
\forall ts, tt, r0s, r0t :: tt == ts && r0s == r0t 
  ==> source_Load_mem_64(ts, t0s) == target_Load_mem_64(tt, t0t)
```

I.e. these loads have the same effect as long as they are the same address and occur
in the same state.

We would want to instantiate this axiom whenever we have two corresponding
source and target loads, but really we only care about those that
are already likely to line up. Instead of letting the SMT solver
decide when to instantiate this axiom we use the control-flow graph, 
and the requirement that transforms must preserve the order and number
of side-effects to instantiate exactly only the instances of the axiom that
the verification will need.

This is done by walking the source and target CFGs in lockstep,
identifying matching side-effects and adding the
body of the axiom as an assertion to the verification condition.

- After this is performed all `SideEffectStatement` are removed from the program.

### Passified Form

Since we have SSA form the semantics of assignment are unneccessary, we replace
every assignment with an `Assume` stating the equality of assignees.

We now have a program consisting only of `Assume` statements.

### SMT

- `TranslationValidate.scala`

- Infer invariant component at each cut and rename for the SSA renaming at the corresponding cut
  - Rename free variables for ssa indexes for synth entry precondition and emit assertion
  - Rename free variables for ssa indexes for synth exit and emit negated assertion
- Add every assume from the passified program to the SMT query
- Add the initial invariant to the SMT query, add the negated exit-invariant to the SMT query.

This is built with `JavaSMT` and Basil's internal SMT builder.


## Debugging

### Validation Failure

When immediate verification is enabled, and `sat` is returned, the validator emits an `.il` file and a 
CFG for a containing a fake representation of the passified product
program. It attempts to annotate the CFG with the model, however note that it often
incorrectly relates source variables to target variables (due to mis-alignment of blocks, assigns, SSA-indexing), 
so this cannot be taken as given.

### Unsoundness

A litmus-test for the soundness of the verification is to generate the unsat core for the dumped SMT query.
If the verification is substantive, the unsat core should contain the entire transition system:
assertions named `source$number` and `tgt$number`.

