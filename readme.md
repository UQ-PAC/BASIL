# BASIL (Boogie Analysis for Secure Information-Flow Logics)

## About

The BASIL tool generates semantically equivalent Boogie source files (`.bpl`) from AArch64/ARM64 
binaries that have been lifted to intermediate formats. Supported input formats are BAP (Binary Analysis Platform) intermediate ADT format, and the `.gts` format produced by [gtirb-semantics](https://github.com/UQ-PAC/gtirb-semantics).

### Example

```sh
$ ./mill run --input src/test/correct/secret_write/clang/secret_write.adt --relf src/test/correct/secret_write/clang/secret_write.relf --spec src/test/correct/secret_write/secret_write.spec --output boogie_out.bpl
[INFO]   [!] Loading Program
[INFO]   [!] Removing external function calls
[INFO]   [!] Stripping unreachable
[INFO]   [!] Removed 11 functions (1 remaining)
[INFO]   [!] Translating to Boogie
[INFO]   [!] Writing file...
$ tail boogie_out.bpl 
    mem, Gamma_mem := memory_store32_le(mem, bvadd64(R9, 52bv64), R8[32:0]), gamma_store32(Gamma_mem, bvadd64(R9, 52bv64), Gamma_R8);
    assert ((bvadd64(R9, 52bv64) == $z_addr) ==> (L(mem, $x_addr) ==> Gamma_x_old));
    assert bvsge32(memory_load32_le(mem, $z_addr), z_old);
    assume {:captureState "%0000033f"} true;
    goto main_basil_return;
  main_basil_return:
    assume {:captureState "main_basil_return"} true;
    return;
}

$ boogie boogie_out.bpl 

Boogie program verifier finished with 4 verified, 0 errors
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
  --analyse                       Run static analysis pass.
  --analysis-results <str>        Log analysis results in files at specified path.
  --analysis-results-dot <str>    Log analysis results in .dot form at specified path.
  --boogie-procedure-rg <str>     Switch version of procedure rely/guarantee checks to emit.
                                  (function|ifblock)
  --boogie-use-lambda-stores      Use lambda representation of store operations.
  --dump-il <str>                 Dump the Intermediate Language to text.
  -h --help                       Show this help message.
  -i --input <str>                BAP .adt file or GTIRB/ASLi .gts file
  --interpret                     Run BASIL IL interpreter.
  -m --main-procedure-name <str>  Name of the main procedure to begin analysis at.
  -o --output <str>               Boogie output destination file.
  --procedure-call-depth <int>    Cull procedures beyond this call depth from the main function
                                  (defaults to Int.MaxValue)
  -r --relf <str>                 Name of the file containing the output of 'readelf -s -r -W'.
  -s --spec <str>                 BASIL specification file.
  --summarise-procedures          Generates summaries of procedures which are used in
                                  pre/post-conditions (requires --analyse flag)
  -t --threads                    Separates threads into multiple .bpl files with given output
                                  filename as prefix (requires --analyse flag)
  -v --verbose                    Show extra debugging logs.
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
./mill test.testOnly '*SystemTests*' -- -z secret_write -z secret_write 
```
Windows:
```
./mill.bat test.testOnly '*SystemTests*' -- -z secret_write -z secret_write 
```

## Open Source License

Copyright 2022 The University of Queensland

Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except in compliance with the License. You may obtain a copy of the License at:

   http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the License for the specific language governing permissions and limitations under the License.
