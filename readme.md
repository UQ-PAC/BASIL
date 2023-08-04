# BAP-to-Boogie Translator

## About
The BAP-to-Boogie Translator generates semantically equivalent Boogie source files (`.bpl`) from AArch64/ARM64 binaries that have been lifted to the BAP (Binary Analysis Platform) intermediate ADT format. 

## Installation and use

### Using Containers 

The docker config can be used to provide bap with the asli-plugin as well as compile the tool itself.

Requirements:

- podman, podman-compose

Note it is recommended to use `podman` rather than docker.

1. To build the images, from the root of the respository run

```
podman-compose build
```

Individual services can be built with `podman compose build $servicename` from the root of the repo. 

The services provided are:
- `bap`
   - To invoke bap on its own: `podman-compose run bap`
- `basil-dev` dev environment containng scala build environment, bap and cross-compilers
   - To compile basil into the current directory using the sbt and scala provided by the docker image: 
      - `podman compose run basil-dev sbt assembly`
   - To enter a shell inside the container
      - `podman compose run basil-build`
- `basil-build`
   - The same as above however containg only the scala build environment and compiled basil
   - To recompile with the currently checked-out repo run `podman-compose build basil-build`
   - To enter a shell inside the container
      - `podman compose run basil-build`
- `basil` precompiled jar file
   - To run the jar inside the docker image `podman-compose run basil $arguments...`
- 


### Native

The tool is OS-independent, but producing input files from a given AArch64 binary is Linux-specific, and all commands given are for Linux. On Windows, WSL2 can be used to run any Linux-specific tasks.

Installing [sbt](https://www.scala-sbt.org/download.html) and [JDK 17](https://openjdk.org/install/) (or higher) is required.

The tool takes as inputs a BAP ADT file (here denoted with `.adt`) and a file containing the output of readelf (here denoted with `.relf`), both created from the same AArch64/ARM64 binary, and outputs a semantically equivalent .bpl Boogie-language source file. The default output file is `boogie_out.bpl`, but the output location can be specified.

To build and run the tool using sbt, use the following command:

`sbt "run file.adt file.relf [file.spec] [output.bpl] [-analyse]"` where the output filename is optional and specification filenames are optional. The specification filename must end in `.spec`.

The `-analyse` flag is optional and enables the static analysis functionality.

The sbt shell can also be used for multiple tasks with less overhead by executing `sbt` and then the relevant sbt commands.

To build a standalone `.jar` file, use the following command:

`sbt assembly`

To compile the source without running it - this helps IntelliJ highlight things properly:

`sbt compile`

The standalone `.jar` can then be executed with the following command:

`./run.sh file.adt file.relf [file.spec] [output.bpl] [-analyse]`

## Generating inputs
The tool takes a `.adt` and a `.relf` file as inputs, which are produced by BAP and readelf, respectively.

[BAP](https://github.com/BinaryAnalysisPlatform/bap) can be installed by following the instructions in the link given.

Given a AArch64/ARM64 binary file (`*.out`), the `.adt` file can be produced by running

`bap *.out -d adt:*.adt`

and the `.relf` file can be produced by running

`readelf -s -r -W *.out > *.relf`.

To cross-compile a C source file to a AArch64 binary, `gcc-aarch64-linux-gnu` must be installed. This can be done with the following commands on Ubuntu:

`sudo apt-get update`

`sudo apt-get install gcc-aarch64-linux-gnu`

The binary (i.e `*.out`) can then be generated from a C source file using:

`aarch64-linux-gnu-gcc *.c -o *.out`

To compile a binary from a C source and immediately generate the required .adt and .relf files, the following command can be used:

`./lift.sh *.c *.adt *.relf` where `*.adt` and `*.relf` are the output file names.

To compile a C source and then run the tool on it, generating the required files first, the following command can be used:

`./run_c.sh *.c [output.bpl]` where the output filename is optional (requires `sbt assembly` first)

To generate the required files from a AArch64 binary and then run the tool on it, the following command can be used:

`./run_binary.sh *.c [output.bpl]` where the output filename is optional (requires `sbt assembly` first)

## Running Boogie on output .bpl

[Boogie](https://github.com/boogie-org/boogie#installation) can be installed by following the instructions in the given link.

Boogie can be run on the output `.bpl` file with the command `boogie *.bpl`. At present, there are no verification conditions, so this is just a syntax check.

### Note about using sbt with IntelliJ

The project uses sbt to build the project. For the most part this should *just work*, and there are IntelliJ run files (located in the .run folder) which automatically compile and run the project.
However, to prevent some issues in IntelliJ, it is necessary to unmark `target/scala-3.1.0/src_managed/` as a sources root folder, in the `File > Project Structure > Modules` dialog.

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
