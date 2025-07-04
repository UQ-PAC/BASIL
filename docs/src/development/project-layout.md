
## Project Layout


The best entrypoint to start reading the code is probably
[src/main/scala/util/RunUtils](https://github.com/UQ-PAC/BASIL/tree/main/src/main/scala/util/RunUtils.scala),
which contains the main execution path and invokes each loading, 
analysis, and translation operation in sequence.


- [src/main](https://github.com/UQ-PAC/BASIL/tree/main/src/main):
    - [scala](https://github.com/UQ-PAC/BASIL/tree/main/src/main/scala): BASIL source
        - [analysis](https://github.com/UQ-PAC/BASIL/tree/main/src/main/scala/analysis): static analysis source, including IR expression evaluation
            - solvers: analysis solver implementation
            - BitVectorEval.scala: evaluation of bitvector operatiosn
        - [bap](https://github.com/UQ-PAC/BASIL/tree/main/src/main/scala/bap): ADT AST definition used in the BAP frontend
        - [specification](https://github.com/UQ-PAC/BASIL/tree/main/src/main/scala/specification): Specification file loading
        - [boogie](https://github.com/UQ-PAC/BASIL/tree/main/src/main/scala/boogie): Boogie AST definition used in the backend
        - [gtirb](https://github.com/UQ-PAC/BASIL/tree/main/src/main/scala/gtirb): Code for parsing gtirb file contents
        - [ir](https://github.com/UQ-PAC/BASIL/tree/main/src/main/scala/ir): Implementation of BASIL IR
            - dsl: IR construction convenience methods
            - Visitor.scala: vistor pattern for IR
            - Program.scala: IR Programs, Procedures, and Blocks 
            - Statement.scala: IR statements and control-flow commands
            - Expr.scala: IR Expressions
            - IRCursor.scala: IR CFG Iterator
        - [translating](https://github.com/UQ-PAC/BASIL/tree/main/src/main/scala/translating): Implementations of translations between IRs
        - [util](https://github.com/UQ-PAC/BASIL/tree/main/src/main/scala/util): BASIL internal utilities, logging, configuration etc.
        - [cfg_visualiser](https://github.com/UQ-PAC/BASIL/tree/main/src/main/scala/cfg_visualiser): dot digraph model for displaying CFGs
    - [antlr4](https://github.com/UQ-PAC/BASIL/tree/main/src/main/antlr4): grammar definitions for Antlr parser generator
    - [protobuf](https://github.com/UQ-PAC/BASIL/tree/main/src/main/protobuf): imported ProtocolBuffer definitions for [gtirb](https://github.com/GrammaTech/gtirb)
- [src/test](https://github.com/UQ-PAC/BASIL/tree/main/src/test): unit test cases
    - correct: System test (SystemTests.scala) examples which should verify
    - incorrect: System test (SystemTests.scala) examples which should not verify
    - make: Makefiles for lifting the System test examples
    - analysis: static analysis tests
    - scala: other unit tests
- [docker](https://github.com/UQ-PAC/BASIL/tree/main/docker): podman container definition files
- [docs](https://github.com/UQ-PAC/BASIL/tree/main/docs): high-level documentation for BASIL development and design. 
  These should be kept up-to-date but should be relatively abstract so that
  implementation changes don't require an overhaul to the documentation.
- [examples](https://github.com/UQ-PAC/BASIL/tree/main/examples), [examples-rg](https://github.com/UQ-PAC/BASIL/tree/main/examples-rg): examples which don't fully work, but we want to keep for reference. 
  These are often not up to date with recent versions of BASIL.
- [scripts](https://github.com/UQ-PAC/BASIL/tree/main/scripts): utility scripts
