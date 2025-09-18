# Translation Validation

Translation validaion is a system for verifying a transform pass preserves the 
behaviour of a Basil IR program.

The translation validator aims to prove that a given run of a transform is trace-preserving.
This property is sufficient to show that security verification on the transformed program
is sufficient to for the property to hold on the original program.

The requirement is resonably strong and includes:

- every possible path through loops corresponds between programs
- the set of observable variables (globals and in/out parameters) in the resulting program take the same
  values on all traces as they do in corresponding traces in the original program
- The exact sequence of side-effects in the source and target program is identical.

For our purposes, side effects include:

- memory stores and loads
- procedure calls
- indirect calls
- control-flow branches (those marked `checkSecurity = true`)

At a high level the translation validation works similarly to regular program verification.
This verification is applied to a 'product program' which combines the program prior to the transform (the *target* program)
and the program after the transform (the *source* program).
This verification is with respect to a specification stating that the source program has the same behaviour
as the target program. We automatically construct this specification using information 
provided by the transform. This is sometimes called a certificate.
We often call this specification an `invariant`, since it closely corresponds to a loop invariant.

> **Note on terminology:**
>
> We term the "*source*" program as the "higher level" or more 
> abstract program that is the *result* of a lifting transform pass.
>
> The "*target*" program refers to the original program, the *input* to the lifting
> transform pass.
>
> This is the reverse of how how inputs and outputs are typically termed in compiler
> validation work.

The translation validation is performed per-procedure, with a simple analysis being used
to infer live variables and procedure frames which summarises interprocedural effects
in the procedure scope.
