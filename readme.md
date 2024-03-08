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

## Installation and use

This is a Scala 3 project using the [mill](mill-build.com) build system, it can be run with `mill run`. 

### Local Development 

The tool itself is a Scala project and is OS-independent.

Furthermore, lifting input files from a given AArch64 binary is Linux-specific, and all commands given are for Linux. 
On Windows, WSL2 may be used to run any Linux-specific tasks.

Installing [mill](https://mill-build.com/mill/Installation_IDE_Support.html) or [sbt](https://www.scala-sbt.org/download.html) and [JDK >= 17](https://openjdk.org/install/) is required.

This can be done via [coursier](https://get-coursier.io/docs/cli-overview).

A mill script is included at `./mill` so it does not need to be installed. For Windows, `./mill.bat` should be used instead.

#### IDE Support

Mill supports the Metals language server and IntelliJ through Build Server Protocol (BSP), which is the recommended use. 

SBT has an intellij plugin in addition to BSP. 

To use specifically Mill's bsp support, delete `.bsp`, run `mill mill.bsp.BSP/install`, and replace the first argument of `argv` in the resulting `.bsp/mill-bsp.json` with the 
command used to run mill (`mill` if mill is installed, or the mill script, either `./mill` on Linux/OSX or `./mill.bat` on Windows). 

```
~ rm -r .bsp
~ ./scripts/setup-bsp.sh 
# create bsp config file
+ ./mill mill.bsp.BSP/install
[2/2] mill.bsp.BSP.install 
Creating BSP connection file: /home/am/Documents/programming/2023/bil-to-boogie-translator/.bsp/mill-bsp.json
# fix config file 
++ perl -pe 's/"argv":\[".*?",/"argv":["$MILL_CMD",/g' .bsp/mill-bsp.json
+ fixed='{"name":"mill-bsp","argv":["./mill","--bsp","--disable-ticker","--color","false","--jobs","1"],"millVersion":"0.11.6","bspVersion":"2.1.0-M7","languages":["scala","java"]}'
```
More information is available [here](https://mill-build.com/mill/Installation_IDE_Support.html#_build_server_protocol_bsp).
However 
### Note about using sbt with IntelliJ

[See Also](https://github.com/UQ-PAC/bil-to-boogie-translator/wiki/Development)

The project uses sbt to build the project. For the most part this should *just work*, and there are IntelliJ run files (located in the .run folder) which automatically compile and run the project.
However, to prevent some issues in IntelliJ, it is necessary to unmark `target/scala-3.1.0/src_managed/` as a sources root folder, in the `File > Project Structure > Modules` dialog.


---

The tool takes as inputs either a BAP ADT file (here denoted with `.adt`) or a `.gts` file produced by [gtirb-semantics](https://github.com/UQ-PAC/gtirb-semantics), as well as a file containing the output of readelf (here denoted with `.relf`), both created from the same AArch64/ARM64 binary, and outputs a semantically equivalent .bpl Boogie-language source file. The default output file is `boogie_out.bpl`, but the output location can be specified.

To build and run the tool using sbt, use the following command:

`sbt "run --input file.{adt, gts} --relf file.relf [--spec file.spec] [--output output.bpl] [--analyse] [--interpret]"` where the output filename is optional and specification filenames are optional. The specification filename must end in `.spec`.

or mill:

`mill run --input file.adt --relf file.relf [--spec file.spec] [--output output.bpl] [--analyse] [--interpret]`.

#### Usage

The `--analyse` flag is optional and enables the static analysis functionality which resolves indirect calls where possible. 

Other flags are listed below:

```
BASIL
  --analyse                       Run static analysis pass.
  --analysis-results <str>        Log analysis results in files at specified path.
  --analysis-results-dot <str>    Log analysis results in .dot form at specified path.
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
  -v --verbose                    Show extra debugging logs.
```

The sbt shell can also be used for multiple tasks with less overhead by executing `sbt` and then the relevant sbt commands.

To build a standalone `.jar` file, use the following command:

`mill assembly`

From `sbt` the resulting `.jar` is located at `target/scala-3.1.0/wptool-boogie-assembly-0.0.1.jar` and from 
`mill` this is `out/assembly.dest/out.jar`.

To compile the source without running it - this helps IntelliJ highlight things properly:

`mill compile`


#### Running Tests

The test suites use [ScalaTest](https://www.scalatest.org/), they can be run via.

```
$ mill test
```

To run a single test suite, for example only the system tests (requires boogie):

```
$ mill.test.testOnly SystemTests
```

To run single tests in from the test suite:

```
$ mill test.testOnly SystemTests -- -z basic_arrays_read -z basic_arrays_write
```

To update the expected basil output files from the test results run

```
$ mill updateExpected
```

This is used to keep track of which tests have passed previously, as well as changes to the basil output.

## Generating inputs (Lifting)

Many lifted examples are already profiled in the tests directory: [src/test/correct](src/test/correct), these instructions
are for if you want to lift new compiled binaries.

The tool takes a `.adt` and a `.relf` file as inputs, which are produced by BAP and readelf, respectively.

### Requirements

- `bap` with the ASLp plugin
- `readelf`
- cross-compiliation toolchains, e.g.:
    - `gcc-13-aarch64-linux-gnu`
    - `clang`

This can be installed using the [nix packages](https://github.com/katrinafyi/pac-nix).

The `dev` podman container also contains these tools and can be used by mounting current directory into the container as described at [docker/readme.md](docker/readme.md). 

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

[Boogie](https://github.com/boogie-org/boogie#installation) can be installed by following the instructions in the given link.

Boogie can be run on the output `*.bpl` file with the command `boogie \useArrayAxioms *.bpl`. 

It is recommended to use Boogie version 3.0.4 and Z3 version 4.8.8 (which is recommended by Boogie). Other versions and combinations may not have been tested.

The `\useArrayAxioms` flag is necessary for Boogie versions 2.16.8 and greater; for earlier versions it can be removed.

### Installing boogie

Boogie can be installed through dotnet and requires dotnet 6.

```
sudo apt-get install dotnet6
dotnet install boogie
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

## Open Source License

Copyright 2022 The University of Queensland

Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except in compliance with the License. You may obtain a copy of the License at:

   http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the License for the specific language governing permissions and limitations under the License.
