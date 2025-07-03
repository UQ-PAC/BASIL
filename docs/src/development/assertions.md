# Assertions

Assertions should be used liberally to check preconditions, post-conditions and invariant we
expect to hold on the code we write. This is useful as it typically requires lower-effort than
writing full unit tests (though is not a substitute for), and provides a sanity check that 
algorithms do what we are intending them to do.

> Asserts should _not_ form part of typical control-flow, and not be an error-handling mechanism
> (e.g. they should not be used for input validation), but should encode assumptions that can only
> be violated in invalid executions or states.

We have specific APIs for asserts, that should be imported with the following line:

```scala
import util.assertion.*
```

### Debug asserts

These are elided by the `--nodebug` flag or `debugMode = false` compilation configuration,
so are allowed to be expensive to compute.

```scala
debugAssert(x, "Optional message")
require(x, "Optional message")
```

This is useful for asserts that are expensive to evaluate.  You should be careful to evaluate the
value `x` inside the call to assert, so that it is not evaluated when the assertion is disabled.

### Unconditional Asserts

```scala
assert(x, "Optional message")
```

This is scala's default assert, and is always executed.

