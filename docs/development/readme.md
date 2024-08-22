# BASIL Development

- [project-layout](project-layout.md) Organisation of the source code
- [editor-setup](editor-setup.md) Guide to basil development in IDEs 
- [tool-installation](tool-installation.md) Guide to lifter, etc. tool installation
- [cfg](cfg.md) Explanation of the old CFG datastructure 


## Scala

Basil is implemented in Scala 3.
Scala is a mixed functional and object-oriented programming language implemented on the JVM. It is a very complicated 
language with a lot of depth, so it is important to carefully chose the implementation complexity introduced. 

Generally, this means favouring simple standard solutions and choosing functional programming in the small scale 
(use filter and map rather than loops), and object-oriented programming in the large scale.

It is recommended to explore the [Scala documentation](https://docs.scala-lang.org/scala3/book/introduction.html).

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

These are lifted via the Makefiles, to add another test simply add a directory, c source file, and optionally specification file and run 

```sh
cd src/test/
make
```

The `config.mk` file in the test directory can be used to exclude unnecessary compilers, and change compilation flags. 
Full details can be found [here](../src/test/readme.md).

### Running Tests

The test suites use [ScalaTest](https://www.scalatest.org/), they can be run via.

```
$ mill test
```

To run a single test suite, for example only the system tests (requires boogie):

```
$ mill.test.testOnly SystemTests
```

To run single tests in from the test suite, they can be selected using globbing on the full test class name with the `testOnly` task:

```
$ mill test.testOnly '*SystemTests*' -- -z basic_arrays_read -z basic_arrays_write
```

To update the expected basil output files from the test results run

```
$ mill updateExpected
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

## Lifting a single example

Many lifted examples are already profiled in the tests directory: [src/test/correct](/src/test/correct), these instructions
are for if you want to lift new compiled binaries.

The tool takes a `.adt` or `.gts` and a `.relf` file as inputs, which are produced by BAP and readelf, respectively.

These instructions are automated in [lift_adt.sh](../../scripts/lift_adt.sh).

### Requirements

- `bap` with the ASLp plugin to produce `.adt`
- or `ddisasm` and `gtirb_semantics` to produce `.gts`
- `readelf`
- cross-compiliation toolchains, e.g.:
    - `gcc-13-aarch64-linux-gnu`
    - `clang`

See [tool-installation](tool-installation.md) for install instructions.

[BAP](https://github.com/BinaryAnalysisPlatform/bap) can be installed by following the instructions in the link given.

Given a AArch64/ARM64 binary file (`*.out`), the `.adt` file can be produced by running

`bap *.out -d adt:*.adt`

and the `.relf` file can be produced by running

`aarch64-linux-gnu-readelf -s -r -W *.out > *.relf`.

To cross-compile a C source file to a AArch64 binary, `gcc-aarch64-linux-gnu` must be installed. This can be done with the following commands on Ubuntu:

`sudo apt-get update`

`sudo apt-get install gcc-aarch64-linux-gnu`

The binary (i.e `*.out`) can then be generated from a C source file using:

`aarch64-linux-gnu-gcc *.c -o *.out`

See [src/test/correct/liftone.sh](https://github.com/UQ-PAC/bil-to-boogie-translator/blob/main/src/test/correct/liftone.sh) for more examples
for flag combinations that work. 

## Verifying the generated Boogie file

[Boogie can be installed through dotnet and requires dotnet 6](tool-installation.md)

Boogie can be run on the output `*.bpl` file with the command `boogie \useArrayAxioms *.bpl`. 

The `\useArrayAxioms` flag is necessary for Boogie versions 2.16.8 and greater; for earlier versions it can be removed.
This improves the verification speed, by using Boogie's axiomatic encoding of arrays, rather than z3's. 
This encoding does not support extensionality, array extensionality can be disabled on newer versions of boogie with:

```
boogie example.bpl /proverOpt:O:smt.array.extensional=false
```

## Other useful commands 

To compile a C file without the global offset table being used in address calculation, use the following command:

`aarch64-linux-gnu-gcc -fno-plt -fno-pic *.c -o *.out`

To produce assembly from the binary, use either of the following commands:

`aarch64-linux-gnu-objdump -d *.out`

`bap *.out -d asm`

To view the hex dump of the data section of the binary:

`readelf -x .data *.out`

To produce a BIR (BAP Intermediate Representation) file (which contains similar information to the BAP ADT file but is more human-readable):

`bap *.out -d:*.bir`

To compile a C file with the stack guard turned off:

`aarch64-linux-gnu-gcc -fno-stack-protector -fno-plt -fno-pic *.c -o *.out`

To produce a translation to BIL (BAP Intermediate Language) for one instruction at a time:

`bap objdump *.out --show-{memory,bil,insn}`

## Managing docker containers

[See docker readme](../../docker/readme.md)

