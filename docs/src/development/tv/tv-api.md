# Translation Validation API

Translation validation is performed by the method `getValidationSMT` on the `TranslationValidator` class. 

We now explain its signature in full, it is provided in full below for context.

```scala


case class TVJob(
  outputPath: Option[String],
  verify: Option[util.SMT.Solver] = None,
  results: List[TVResult] = List(),
  debugDumpAlways: Boolean = false,
  /* minimum number of statements in source and target combined to trigger case analysis */
  splitLargeProceduresThreshold: Option[Int] = Some(60)
)

object TranslationValidator:
  def forTransform[T](
    transformName: String,
    transform: Program => T,
    invariant: T => InvariantDescription = (_: T) => InvariantDescription()
  ): ((Program, TVJob) => TVJob)
```

This returns a anonymous function which runs the provided transform on a program, passes its result to the invariant function
to produce a description of the transform, then runs the tanslation validation, returning a copy of the `TVJob`
including the additional validation results.

The invariants are specified with the `InvariantDescription` type, which can be derived from the output of the transform functor.

```scala

/**
 * Describes the mapping from a variable in one program to an expression in the other, at a specific block and procedure.
 */
type TransformDataRelationFun = (ProcID, Option[BlockID]) => (Variable | Memory) => Seq[Expr]

case class InvariantDescription(
  /** The way live variables at each cut in the source program relate to equivalent expressions or variables in the target.
   *
   * NOTE: !!! The first returned value of this is also used to map procedure call arguments in the source
   * program to the equivalent arguments in the target program.
   *  */
  renamingSrcTgt: TransformDataRelationFun = (_, _) => e => Seq(e),

  /**
   * Describes how live variables at a cut in the target program relate to equivalent variables in the source.
   *
   */
  renamingTgtSrc: TransformDataRelationFun = (_, _) => _ => Seq(),

  /**
   * Set of values of [ir.Assert.label] for assertions introduced in this pass, whose should
   * be ignored as far as translation validation is concerned.
   */
  introducedAsserts: Set[String] = Set()
) {
```

### Shape-Preserving Transforms

However, for simple transforms---those which are *shape-preserving*--and perform only
limited code motion, can use the default relation between source and target programs.

> **Shape preserving:**
> A transform which does not change the 'shape' of the program state: variables and 
> memory are not renamed, added or removed and are used in the same way

This default invaraint states that every variable in the source corresponds to the same 
variable in the target.

An example of this is the identity transform, which makes no changes:

```scala
def nop(p: Program) = {
  // execute the transform and write validation queries to folder tvsmt
  TranslationValidator.forTransform("NOP", p => p)(p, TVJob(Some("tvsmt")))
}
```

This validates with the default invariant `() => InvariantDescription`

Even dead-code eliminiation can be handled with the default invariant.

### Non-Shape-Preserving 

For sophisticated transforms more information may be provided to the validation framework
to generate the verification invariant through the parameters `renamingSrcTgt` and `flowFacts`.

1. source-to-target variable relations; to relate possibly renamed variables (`renamingSrcTgt` parameter)
  - !! This is also used to figure out how to match procedure call parameters between the target and source programs.
2. source-to-target variable relations; used in copyprop to pass definitions across cuts for variables live in the
  target program but not live in the source program.

## Introducing Assumptions via Later-Verified Assertions

see also: [lifter-assertions](assumptions.md)

For transforms that introduce assumptions by adding assertions to be discharged later, the
framework allows these to be temporarily ignored during validation. 

The verification then verifies the translation, but ignores traces differing only by the failure 
of an assertion introduced by the transform. Since these traces will later be verified to not exist,
we can soundly ignore them in the translation validation.

```scala
TranslationValidator.forTransform(
  "AssumeCallPreserved",
  transforms.CalleePreservedParam.transform,
  asserts => InvariantDescription(introducedAsserts = asserts.toSet)
)
```

# Soundness of Translation Validation

The specified assertion becomes an inductive loop invariant, so if it is not strong enough
the usual failure mode is for it not to verify, either due to not being inductive 
or not being strong enough to establish the post-condition (equivalence of the procedure's post-state).

However, since we currently do not implement specification validation it is possible to inject
`false` as a specification and this will vacuously verify.
