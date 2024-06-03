# BASIL Development

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

## Development tasks

### Debugging

Printing the IR

- dumping the IR
- dumping an analysis result

- Printing an analysis result

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

To achieve this for a new result use the following method defined in `RunUtils.scala`

```scala
def printAnalysisResults(prog: Program, result: Map[CFGPosition, _]): String 
```


## Tests

### Unit tests

We use the [ScalaTest](https://www.scalatest.org/) unit testing framework. Example unit tests can be found in [src/test/scala](../src/test/scala/).

The [dsl](basil-ir.md#constructing-programs-in-code) can be used to construct simple example BASIL IR programs, which can then be fed through into the whole pipeline via `IRLoading.load()` in
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

- [see readme](../readme.md#running-tests)

## Performance profiling

The performance target for the static analyses is that we can run through the entire 
[cntlm](https://github.com/versat/cntlm) binary in a reasonable amount of time (seconds),
depending on the expected performance of the analysis involved. 
Loading cntlm requires increasing the heap size by providing the `-Xmx8G` flag.

IntelliJ professional (which can be obtained for free by students) includes a performance profiler.

Alternatively, [async-profiler](https://github.com/async-profiler/async-profiler) can be used to produce a 
[flame graph](https://brendangregg.com/flamegraphs.html) showing the hot-spots in the program. Download the library from 
the releases tab, compile a basil .jar and run the jar with the following arguments.

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
