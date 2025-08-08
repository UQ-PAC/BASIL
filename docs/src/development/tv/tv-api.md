# Translation Validation API

Translation validation is performed by the method `getValidationSMT` on the `TranslationValidator` class. 

We now explain its signature in full, it is provided in full below for context.

```scala
  def getValidationSMT(
    config: TVJob,   // configuration
    runName: String, // name of the run for bookkeeping
    invariantRenamingSrcTgt: TransformDataRelationFun = _ => e => Some(e),
    flowFacts: TransformTargetTargetFlowFact = _ => Map(),
    introducedAsserts: Set[String] = Set()
  ): TVJob
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
def identityTransform(config: TVJob, p: Program) : TVJob = {
  def transform(p: Program) = ()
  val (validator, _) = validatorForTransform(transform)(p)
  validator.getValidationSMT(config, "Identity")
}
```

Even dead-code eliminiation can be handled with the default invariant.

### Non-Shape-Preserving 

For sophisticated transforms more information may be provided to the validation framework
to generate the verification invariant through the parameters `invariantRenamingSrcTgt` and `flowFacts`.

1. target-to-source variable relations; to relate possibly renamed variables (`invariantRenamingSrcTgt` parameter)
2. target-to-target variable relations; to describe static analysis facts motivating the transform (`flowFacts` parameter)

An example which requires this is the parameter analysis which lifts all global variables to local variables and
introduces procedure parameters, we provide the validator which a function which translates variables
in the target program to expressions in the source program:

```scala
def parameters(config: TVJob, p: Program) = {
  val (validator, _) = validatorForTransform(p => transforms.liftProcedureCallAbstraction(p, None))(p)

  // describes how the source and target programs relate to eachother after the transform 
  def sourceToTarget(b: Option[BlockID])(v: Variable | Memory): Option[Expr] = v match {
    case LocalVar(s"${i}_in", t, 0) => Some(GlobalVar(s"$i", t))
    case LocalVar(s"${i}_out", t, 0) => Some(GlobalVar(s"$i", t))
    case LocalVar(n, t, 0) => Some(GlobalVar(n, t))
    case g => Some(g)
  }

  validator.getValidationSMT(config, "Parameters", sourceToTarget)
}
```

## Introducing Assumptions via Later-Verified Assertions

For transforms that introduce assumptions by adding assertions to be discharged later, the
framework allows these to be temporarily ignored during validation. 

The verification then verifies the translation, but ignores traces differing only by the failure 
of an assertion introduced by the transform. Since these traces will later be verified to not exist,
we can soundly ignore them in the translation validation.

```scala
def assumePreservedParams(config: TVJob, p: Program) : TVJob = {
  val (validator, asserts) = validatorForTransform(transforms.CalleePreservedParam.transform)(p)
  validator.getValidationSMT(config, "AssumeCallPreserved", introducedAsserts = asserts.toSet)
}
```

# Soundness of Translation Validation

The specified assertion becomes an inductive loop invariant, so if it is not strong enough
the usual failure mode is for it not to verify, either due to not being inductive 
or not being strong enough to establish the post-condition (equivalence of the procedure's post-state).

However, since we currently do not implement specification validation it is possible to inject
`false` as a specification and this will vacuously verify.
