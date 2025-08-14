# BASIL Development

[API Documentation](../api/)

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
3. Install the neccessary tools [here](../usage.md), it may be useful to try
   lifting examples, or just looking at existing examples in the
   [src/test/correct](https://github.com/UQ-PAC/BASIL/tree/main/src/test/correct) directory.
4. Use the below as a guide to common development tasks as they may arise.

## Development tasks

Mill should always be run from the git root directory (`git rev-parse --show-toplevel`).

## Building

To build the project you can use `./mill build`. Often the incremental compilation database won't be properly invalidated on
`git switch`/`git checkout`/`git pull`: in these cases `./mill clean` is needed to trigger a full rebuild.

To build a standalone `.jar` file, use `./mill assembly`. This can be found at `out/assembly.dest/out.jar`.

### Debugging

Printing an analysis result

Run basil with the `--analysis-result <filepath>` flag to print the results of the analyses to files.
The `--analysis-result-dot <filepath>` does the same, but outputs a graphviz digraph, which can be viewed by pasting it into [edotor.net](https://edotor.net), or compiling with `dot`.

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

## Code Formatting

All pull requests must conform to the style produced by `scalafmt`, and imports must be organised by `scalafix`.
This is checked automatically in CI. Locally, it can be run with:
```bash
./mill fmt
```
This will first organise the imports with scalafix, then run scalafmt to fix indentation and formatting.

To run one of these tools manually, you can use:
```bash
./mill fix  # for scalafix in main basil code
./mill test.fix  # for scalafix in basil test code
./mill scalafmt.reformat  # for scalafmt across basil + tests
```
To match the style expected by CI, you should run scalafmt _after_ scalafix, as
scalafix can introduce some badly-formatted code which scalafmt will want to fix.

## Tests

See [testing](testing.md).

