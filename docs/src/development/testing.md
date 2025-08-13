Testing
=======

Testing is crucial for validating new features and preventing regressions.
It is important that tests are written for any new features, as well as any
identified bugs.
It is important that we should have a clear understanding of what each test
is testing, whether it currently passes and, if not, why it is currently
not working.
This page will describe some tools in the codebase which help with developing
and maintaining test cases.

## Running test suites

`mill test.testOnly` can be used to run a particular test suite.
Test suite names are glob matched against its argument.

For example, to run all the SystemTest variants use:
```bash
./mill test.testOnly 'SystemTest*'
```

`./mill -w test.testOnly ...` can be used to re-run
a test suite on every Scala change.
This is useful for test-driven development.

The list of test suites can be found using
```bash
./mill show test.discoveredTestClasses
```

To run only tests from a specific tag, you can use
```bash
./scripts/scalatest.sh -o -n test_utils.tags.UnitTest
```
Note that the tag name must be fully-qualified (i.e., including the package name).
See [the Scalatest runner docs](https://www.scalatest.org/user_guide/using_the_runner) or the `scalatest.sh` file
for more options.

To run all non-disabled tests (i.e., all tests which CI will run),
you can use
```bash
./scripts/scalatest.sh -o -l test_utils.tags.DisabledTest
```

Finally, `./mill test` will run _all_ tests, including disabled ones.
This is generally less useful, as some test suites are known to be broken.

Writing test cases
------------------

We use the [ScalaTest](https://www.scalatest.org/) unit testing framework. Example unit tests can be found in [/src/test/scala](https://github.com/UQ-PAC/BASIL/tree/main/src/test/scala).

Test cases are written in Scalatest using its
[`AnyFunSuite`](https://www.scalatest.org/scaladoc/3.1.2/org/scalatest/funsuite/AnyFunSuite.html) style.
See the AnyFunSuite documentation or existing test cases for syntax and examples.

### Exporting IR structures into test cases

The [dsl](basil-ir.md#constructing-programs-in-code) can be used to construct simple example BASIL IR programs, which can then be fed through into the whole pipeline via `IRLoading.load()` in

Often, you might have found a particular Basil IR program which demonstrates some bug in the code.
It is good practice to extract this into a test case, both to validate the fix and ensure the bug doesn't reoccur.
To help with this, a Basil IR program can be converted to
a Scala literal by using the [ToScala](https://github.com/UQ-PAC/BASIL/blob/main/src/main/scala/ir/dsl/ToScala.scala)
trait.

To do this, first `import ir.dsl.given`, then you can use the `.toScala` extension method on programs, procedures, or blocks.
This gives you a string which is valid Scala code.
This can be copied and pasted into a unit test.
When executed, the Scala code to will re-construct that Basil IR structure using the DSL.

### Tagging test suites

[Tags](https://www.scalatest.org/scaladoc/3.2.1/org/scalatest/Tag.html)
are used to categorise test classes based on, roughly, the kind of test (e.g., unit tests or system (end-to-end) tests).
Each test suite should be tagged with one of the
[`@test_util.tags.*Test`](https://github.com/UQ-PAC/BASIL/tree/main/src/test/scala/test_util/tags) tags,
placed on the line before the AnyFunSuite class declaration.
A test suite may, additionally, be tagged with one or more of the supplementary tags (those not ending in Test).

### Dynamic tests

Note that the `test("test name")` method can be written anywhere within a AnyFunSuite body, including
within loops or conditionals.
This allows you to dynamically generate test cases, as in
Basil's [`SystemTests`](https://github.com/UQ-PAC/BASIL/blob/main/src/test/scala/SystemTests.scala).
This should be used sparingly.


Maintaining test cases
----------------------
Over time, test cases might break due to code changes or refactoring.
Of course, failing tests should be fixed as soon as possible.
However, this is not always possible - maybe a test case relies on features not yet implemented.
It might be reasonable to allow a test to fail for a period of time until the fixes are ready.
In these cases, it is important that tests which are known/expected to fail are clearly marked
and the reason for their failure should be recorded in the code.

Generally, the strategy is that failing tests should still be executed.
They should be annotated so that they are allowed to fail, but if they start passing,
that should raise an error until the annotation is removed.
This allows the test code to be an accurate record of the expected outcome of each test.


### pendingUntilFixed (for expected failures)

[`pendingUntilFixed`](https://www.scalatest.org/scaladoc/3.2.3/org/scalatest/Assertions.html#pendingUntilFixed(f:=%3EUnit)(implicitpos:org.scalactic.source.Position):org.scalatest.Assertionwithorg.scalatest.PendingStatement) should be used to mark a block of code (typically containing an assertion) as one which is currently expected to fail. This should be used to record test cases which fail due to not-yet-implemented features or known bugs. If a change is made and the code no longer fails, this will cause the test to fail until the pendingUntilFixed is removed.

It should be used within a `test() { ... }` block like so, with a comment documenting the cause of failure and expected future resolution.
```scala
test("1 == 2 sometime soon?") {
  assert(1 == 1, "obviously")

  // broken until we fix maths
  pendingUntilFixed {
    assert(1 == 2, "todo")
  }
}
```

- Tests can be marked as ignored by replacing `test()` with `ignore()`. An entire suite can be marked as ignored with the `@org.scalatest.Ignore` annotation.

### TestCustomisation (for dynamically-generated tests)

For some tests, particularly those which are dynamically-generated by a loop, it is not practical to add a `pendingUntilFixed` block
into the test body.
For these cases, there is a `trait TestCustomisation` to help with customising dynamically-generated tests
based on their test case name (for system tests, this includes the file path and compiler/lifter options).

To use this, the test suite class should be made to extend [TestCustomisation](https://github.com/UQ-PAC/BASIL/blob/main/src/test/scala/test_util/TestCustomisation.scala).
This defines an abstract method customiseTestsByName which controls the mode of each test case.
```scala
@test_util.tags.UnitTest
class ProcedureSummaryTests extends AnyFunSuite, TestCustomisation {

  override def customiseTestsByName(name: String) = {
    name match {
      case "test a" => Mode.NotImplemented("doesn't seem to work yet")
      case _ => Mode.Normal
    }
  }

  test("test a") {
    assert(false);
  }

  test("test b") {
    assert(true);
  }
}
```
Test cases can be marked as retry, disabled, not implemented, or temporary failure
(see TestCustomisation source file for more details).
This modifies the behaviour of the test case and prints helpful output when running the test. For example:
```c
- correct/malloc_with_local3/clang:BAP (pending)
  + NOTE: Test case is customised with: "ExpectFailure(previous failure was: Expected verification success, but got failure. Failing assertion is: assert (load37_1 == R30_in))"

  + Failing assertion ./src/test/correct/malloc_with_local3/clang/malloc_with_local3_bap.bpl:264
  + 261 |     load36_1, Gamma_load36_1 := memory_load64_le(mem_9, bvadd64(R31_in, 18446744073709551600bv64)), (gamma_load64(Gamma_mem_9, bvadd64(R31_in, 18446744073709551600bv64)) || L(bvadd64(R31_in, 18446744073709551600bv64)));
    262 |     call rely();
    263 |     load37_1, Gamma_load37_1 := memory_load64_le(mem_10, bvadd64(R31_in, 18446744073709551608bv64)), (gamma_load64(Gamma_mem_10, bvadd64(R31_in, 18446744073709551608bv64)) || L(bvadd64(R31_in, 18446744073709551608bv64)));
 >  264 |     assert (load37_1 == R30_in); //is returning to caller-set R30
    265 |     goto printCharValue_2260_basil_return;
    266 |   printCharValue_2260_basil_return:
    267 |     assume {:captureState "printCharValue_2260_basil_return"} true;
```

```c
- analysis_differential:malloc_with_local/gcc_O2:GTIRB (pending)
  + NOTE: Test case is customised with: "ExpectFailure(needs printf_chk)"

  + STDOUT: ""
```

Modes which expect failure (temp failure and not implemented) will show as "pending" when
executing scalatest.
The disabled mode will show as "cancelled".
Both of these will be output in yellow text if your console is using colour.

