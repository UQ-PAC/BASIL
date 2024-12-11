# BASIL Development

- [project-layout](project-layout.md) Organisation of the source code
- [editor-setup](editor-setup.md) Guide to basil development in IDEs 
- [tool-installation](tool-installation.md) Guide to lifter, etc. tool installation
- [scala](scala.md) Advice on Scala programming.
- [cfg](cfg.md) Explanation of the old CFG datastructure 


## Scala

Basil is implemented in Scala 3.

See also: [Scala Gotchas](scala.md).

Scala is a mixed functional and object-oriented programming language implemented on the JVM. It is a very complicated 
language with a lot of depth, so it is important to carefully chose the implementation complexity introduced. 

Generally, this means favouring simple standard solutions and choosing functional programming over stateful object oriented style 
(use filter and map rather than loops), prefer immutable case classes and enums to regular mutable classes, etc.

ADTs and functions between ADTs are all you need to solve most problems. Most
things more complicated than this make the code unnecessarily difficult to maintain. 

It is recommended to explore the [Scala documentation](https://docs.scala-lang.org/scala3/book/introduction.html).
There is also the incomplete [Scala 3 language specification](https://github.com/scala/scala3/tree/main/docs/_spec), 
which contains details not present in the documentation, but is not completely updated from Scala 2.

Some general advice:

- Prefer [Enums](https://docs.scala-lang.org/scala3/book/types-adts-gadts.html) over inheritance trees
- Use functional programming over imperative wherever possible
- Prefer immutable case classes to regular classes wherever possible
- Don't unneccessarily use generics or type aliases 
- Correct code should not require explicit casts (`.asInstanceOf`)

#### Code style

We do not have a strict code style however 

- Use two spaces for indentation
- Use `{}` braces rather than purely indentation-based scoping
- Do not use `new` or semicolons

## Getting Started

1. To set up an editor for Scala development see [editor-setup](editor-setup.md).
2. Become familiar with the [project structure](project-layout.md) to start understanding the code.
3. Install the neccessary tools [here](tool-installation.md), it may be useful to try
   lifting examples, or just looking at existing examples in the 
   [src/test/correct](../../src/test/correct) directory.
4. Use the below as a guide to common development tasks as they may arise. 

## Development tasks

## Building

The sbt shell can also be used for multiple tasks with less overhead by executing `sbt` and then the relevant sbt commands.

To build a standalone `.jar` file, use the following command:

`mill assembly`

From `sbt` the resulting `.jar` is located at `target/scala-3.1.0/wptool-boogie-assembly-0.0.1.jar` and from 
`mill` this is `out/assembly.dest/out.jar`.

To compile the source without running it - this helps IntelliJ highlight things properly:

`mill compile`

This is used to keep track of which tests have passed previously, as well as changes to the basil output.

### Debugging

Printing an analysis result

Run basil with the `--analysis-result <filepath>` flag to print the results of the analyses to files.
The `--analysis-result-dot <filepath>` does the same, but outputs a graphviz digraph, which can be viewed by pasting it into [edotor.net](edotor.net), or compiling with `dot`.

This prints the program with the abstract-domain lattice value at each program point following the `::`. 

```
Procedure main
  Block $main$__0__$kIwlZLaTRp6qaf6BnFzwlA
    1816$0: Register(R9, bv64) := 69632bv64
        :: Map(Register(R31, bv64) -> Bottom)
...
```

To achieve this for a new result in development use the following method defined in `RunUtils.scala`

```scala
def printAnalysisResults(prog: Program, result: Map[CFGPosition, _]): String 
```

## Tests

### Unit tests

We use the [ScalaTest](https://www.scalatest.org/) unit testing framework. Example unit tests can be found in [src/test/scala](../src/test/scala/).

The [dsl](../basil-ir.md#constructing-programs-in-code) can be used to construct simple example BASIL IR programs, which can then be fed through into the whole pipeline via `IRLoading.load()` in
`RunUtils.scala`. Prefer to write tests that depend only on the single piece of code under test rather than the whole BASIL translation. 

### Integration tests

These are the `SystemTests.scala` test case with the files present in `src/test/correct` for examples that should verify and `src/test/incorrect`
for examples that should not verify.

These are run via the Makefiles in src/test.
A Docker image is used to compile and lift examples in a reproducible way.
See [src/test/readme.md](../src/test/readme.md) for details.

### Running Tests

The test suites use [ScalaTest](https://www.scalatest.org/).

To run the primary SystemTests suites (SystemTestsBAP and SystemTestsGTIRB) (which require Boogie):

```
$ mill.test.testOnly 'SystemTests*'
```

To run a single test from a test suite, it can be selected using globbing on the full test class name with the `testOnly` task:

```
$ mill test.testOnly 'SystemTestsBAP' -- -z basic_arrays_read/gcc:BAP
```

To update the expected BASIL output files from the SystemTests results run:

```
$ mill updateExpected
```

To run another test suite, just use the name of the class containing the test suite (in this case LiveVarsAnalysisTests):

```
$ mill.test.testOnly 'LiveVarsAnalysisTests'
```

To list all test suites:

```
$ mill.test.testOnly * -- -t ''
```

## Performance profiling

While the first priority is correctness, the performance target for the static 
analyses is that we can run through the entire 
[cntlm](https://github.com/versat/cntlm) binary in a reasonable amount of time 
(seconds), depending on the expected performance of the analysis involved. 
Loading cntlm requires increasing the heap size by providing the `-Xmx8G` flag.

IntelliJ professional (which can be obtained for free by students) includes a performance profiler.

Alternatively, [async-profiler](https://github.com/async-profiler/async-profiler) can be used to produce a 
[flame graph](https://brendangregg.com/flamegraphs.html) showing the hot-spots in the program. Download the library from 
the [releases tab](https://github.com/async-profiler/async-profiler/releases), compile a basil .jar with `mill assembly` and run the jar with the following arguments.

Instructions for Linux and Mac:

```sh
mill assembly
java -agentpath:$YOUR_PATH/async-profiler-2.9-linux-x64/build/libasyncProfiler.so=start,event=cpu,file=profile.html -Xmx8G -jar out/assembly.dest/out.jar -i examples/cntlm-new/cntlm-new.adt -r examples/cntlm-new/cntlm-new.relf --analyse;
firefox profile.html
```

You may have to give it permission to collect perf events

```sh
sudo sysctl kernel.perf_event_paranoid=1
sudo sysctl kernel.kptr_restrict=0
```


## Managing docker containers

[See docker readme](../../docker/readme.md)

