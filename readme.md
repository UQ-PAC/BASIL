# BASIL (Boogie Analysis for Secure Information-Flow Logics)

## About

The BASIL tool generates semantically equivalent Boogie source files (`.bpl`) from AArch64/ARM64 
binaries that have been lifted to intermediate formats. Supported input formats are BAP (Binary Analysis Platform) intermediate ADT format, and the `.gts` format produced by [gtirb-semantics](https://github.com/UQ-PAC/gtirb-semantics).

### Example

Example of a direct translation:

```sh
$ mill run --load-directory-bap src/test/correct/secret_write/gcc -o test.bpl --verify 
[70/70] run 
[INFO]   Found src/test/correct/secret_write/gcc/secret_write.adt src/test/correct/secret_write/gcc/secret_write.relf src/test/correct/secret_write/secret_write.spec
[INFO]   [!] Loading Program
[INFO]   [!] Removing external function calls
[INFO]   [!] Stripping unreachable
[INFO]   [!] Removed 11 functions (1 remaining)
[INFO]   [!] Translating to Boogie
[INFO]   Writing output
[INFO]   Running boogie

Boogie program verifier finished with 4 verified, 0 errors
[INFO]   PerformanceTimer Verify [Finish]: 816ms
$ tail test.bpl 
    assert ((R0 == $z_addr) ==> (L(mem, $x_addr) ==> Gamma_x_old));
    assert bvsge32(memory_load32_le(mem, $z_addr), z_old);
    assume {:captureState "%000003b7"} true;
    R0, Gamma_R0 := 0bv64, true;
    goto main_1812_basil_return;
  main_1812_basil_return:
    assume {:captureState "main_1812_basil_return"} true;
    return;
}
```

Example using analysis:

- Infer separate stack/memory variables (--analyse)
- Resolve indirect calls (--memory-regions dsa)
- Simplify code: (--simplify)
  - Fold constant expressions
  - Simplify branch flag calculations to (in)equality conditions
  - Remove dead code

```
$ mill run --load-directory-gtirb src/test/correct/functionpointer/clang --simplify --analyse --memory-regions mra --verify
[INFO]   Found src/test/correct/functionpointer/clang/functionpointer.gts src/test/correct/functionpointer/clang/functionpointer.relf src/test/correct/functionpointer/functionpointer.spec
[INFO]   [!] Loading Program
[INFO]   [!] Removing external function calls
[INFO]   [!] Running Static Analysis
[INFO]   [!] Replacing Indirect Calls
[INFO]   [!] Generating Procedure Summaries
[INFO]   [!] Running Static Analysis
[INFO]   [!] Replacing Indirect Calls
[INFO]   [!] Generating Procedure Summaries
[INFO]   [!] Running Writes To
[INFO]   [!] Running Symbolic Access Analysis
[INFO]   [!] Running DSA Analysis
[INFO]   [!] Finished indirect call resolution after 1 iterations
[INFO]   [!] Running Simplify
[INFO]   [!] Simplify :: DynamicSingleAssignment
[INFO]   DSA 29 ms 
[INFO]   [!] Simplify :: Expr/Copy-prop Transform
[INFO]   [!] Simplify :: Dead variable elimination
[INFO]   [!] Simplify :: Merge empty blocks
[INFO]   CopyProp 72 ms 
[INFO]   [!] Simplify :: finished
[INFO]   [!] Stripping unreachable
[INFO]   [!] Removed 17 functions (4 remaining)
[INFO]   [!] Translating to Boogie
[INFO]   Writing output
[INFO]   Running boogie

Boogie program verifier finished with 5 verified, 0 errors
[INFO]   PerformanceTimer Verify [Finish]: 622ms
```


---

The tool takes as inputs either a BAP ADT file (here denoted with `.adt`) or a `.gts` file produced by [gtirb-semantics](https://github.com/UQ-PAC/gtirb-semantics), as well as a file containing the output of readelf (here denoted with `.relf`), both created from the same AArch64/ARM64 binary, and outputs a semantically equivalent .bpl Boogie-language source file. The default output file is `boogie_out.bpl`, but the output location can be specified.

To build and run the tool use one of the following commands:

sbt:
```
sbt "run --input file.{adt, gts} --relf file.relf [--spec file.spec] [--output output.bpl] [--analyse] [--interpret]"
```
mill (Mac OS X / Linux):
```
./mill run --input file.adt --relf file.relf [--spec file.spec] [--output output.bpl] [--analyse] [--interpret]
```
mill (Windows): 
```
./mill.bat run --input file.adt --relf file.relf [--spec file.spec] [--output output.bpl] [--analyse] [--interpret]
```
The output filename is optional and specification filenames are optional. The specification filename must end in `.spec`.

#### Usage

The `--analyse` flag is optional and enables the static analysis functionality which resolves indirect calls where possible. 

Other flags are listed below:

```
BASIL
  --load-directory-bap <str>      Load relf, adt, and bir from directory (and spec from parent
                                  directory)
  --load-directory-gtirb <str>    Load relf, gts, and bir from directory (and spec from parent
                                  directory)
  -i --input <str>                BAP .adt file or GTIRB/ASLi .gts file
  -r --relf <str>                 Name of the file containing the output of 'readelf -s -r -W'.
  -s --spec <str>                 BASIL specification file.
  -o --output <str>               Boogie output destination file.
  --boogie-use-lambda-stores      Use lambda representation of store operations.
  --boogie-procedure-rg <str>     Switch version of procedure rely/guarantee checks to emit.
                                  (function|ifblock)
  -v --verbose                    Show extra debugging logs (the same as -vl log)
  --vl <str>                      Show extra debugging logs for a specific logger (log.debugdumpir,
                                  log.analysis.vsa, log.simplify, log, log.analysis,
                                  log.analysis.steensgaard, log.analysis.mra).
  --analyse                       Run static analysis pass.
  --interpret                     Run BASIL IL interpreter.
  --dump-il <str>                 Dump the Intermediate Language to text.
  -m --main-procedure-name <str>  Name of the main procedure to begin analysis at.
  --procedure-call-depth <int>    Cull procedures beyond this call depth from the main function
                                  (defaults to Int.MaxValue)
  --trim-early                    Cull procedures BEFORE running analysis
  -h --help                       Show this help message.
  --analysis-results <str>        Log analysis results in files at specified path.
  --analysis-results-dot <str>    Log analysis results in .dot form at specified path.
  -t --threads                    Separates threads into multiple .bpl files with given output
                                  filename as prefix (requires --analyse flag)
  --parameter-form                Lift registers to local variables passed by parameter
  --summarise-procedures          Generates summaries of procedures which are used in
                                  pre/post-conditions (requires --analyse flag)
  --simplify                      Partial evaluate / simplify BASIL IR before output (implies
                                  --parameter-form)
  --validate-simplify             Emit SMT2 check for validation of simplification expression
                                  rewrites 'rewrites.smt2'
  --verify                        Run boogie on the resulting file
  --memory-regions <str>          Performs static analysis to separate memory into discrete regions
                                  in Boogie output (requires --analyse flag) (mra|dsa)
  --no-irreducible-loops          Disable producing irreducible loops when --analyse is passed (does
                                  nothing without --analyse)
```

For more information see [docs/usage](docs/usage.md).

## Introduction

See [docs](docs).

## Development Setup

See [docs/development](docs/development)

## Getting started

1. Install [scala](/docs/development/tool-installation.md)
2. Install [boogie](/docs/development/tool-installation.md)
3. To a single system test case :

Mac OS X / Linux: 
```
./mill test.testOnly 'SystemTestsBAP' -- -z correct/secret_write/clang:BAP
```
Windows:
```
./mill.bat test.testOnly 'SystemTestsBAP' -- -z correct/secret_write/clang:BAP
```

## Open Source License

Copyright 2022 The University of Queensland

Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except in compliance with the License. You may obtain a copy of the License at:

   http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the License for the specific language governing permissions and limitations under the License.
