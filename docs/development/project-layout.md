
## Project Layout

The best entrypoint to start reading the code is probably
[src/main/scala/util/RunUtils](../../src/main/scala/util/RunUtils.scala),
which contains the main execution path and invokes each loading, 
analysis, and translation operation in sequence.


- [src/main](../../src/main):
    - [scala](../../src/main/scala): BASIL source
        - [analysis](../../src/main/scala/analysis): static analysis source, including IR expression evaluation
            - solvers: analysis solver implementation
            - BitVectorEval.scala: evaluation of bitvector operatiosn
        - [bap](../../src/main/scala/bap): ADT AST definition used in the BAP frontend
        - [specification](../../src/main/scala/specification): Specification file loading
        - [boogie](../../src/main/scala/boogie): Boogie AST definition used in the backend
        - [gtirb](../../src/main/scala/gtirb): Code for parsing gtirb file contents
        - [ir](../../src/main/scala/ir): Implementation of BASIL IR
            - dsl: IR construction convenience methods
            - Visitor.scala: vistor pattern for IR
            - Program.scala: IR Programs, Procedures, and Blocks 
            - Statement.scala: IR statements and control-flow commands
            - Expr.scala: IR Expressions
            - IRCursor.scala: IR CFG Iterator
        - [translating](../../src/main/scala/translating): Implementations of translations between IRs
        - [util](../../src/main/scala/util): BASIL internal utilities, logging, configuration etc.
        - [cfg_visualiser](../src/main/scala/cfg_visualiser): dot digraph model for displaying CFGs
    - [antlr4](../../src/main/antlr4): grammar definitions for Antlr parser generator
    - [protobuf](../../src/main/protobuf): imported ProtocolBuffer definitions for [gtirb](https://github.com/GrammaTech/gtirb)
- [src/test](../../src/test): unit test cases
    - correct: System test (SystemTests.scala) examples which should verify
    - incorrect: System test (SystemTests.scala) examples which should not verify
    - make: Makefiles for lifting the System test examples
    - analysis: static analysis tests
    - scala: other unit tests
- [docker](../../docker): podman container definition files
- [docs](../../docs): high-level documentation for BASIL development and design. 
  These should be kept up-to-date but should be relatively abstract so that
  implementation changes don't require an overhaul to the documentation.
- [examples](../../examples), [examples-rg](../../examples-rg): examples which don't fully work, but we want to keep for reference. 
  These are often not up to date with recent versions of BASIL.
- [scripts](../../scripts): utility scripts
