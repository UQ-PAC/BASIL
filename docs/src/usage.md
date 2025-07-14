# Usage

This document covers using the BASIL pipeline to lift and verify binaries,
from the perspective of a user of the tool.
It may also be useful for BASIL developers as a reference for how to lift
new binaries for manual testing.

## Running Basil

Basil needs a JVM(>17), recommended JDK-21, which can be installed using system packages.

- e.g. `sudo apt-get install openjdk-21-jdk`

On first run `./mill` (on linux/windows) or `./mill.bat` (on Windows) should bootstrap Scala automatically. Otherwise it can be set up through [coursier](https://get-coursier.io/docs/cli-installation).

### Test Dependencies

Both unit tests and system tests will require a working installation of the Boogie verifier backend and Z3 SMT solver.

These can be installed through [nix](https://github.com/katrinafyi/pac-nix?tab=readme-ov-file#first-time) (recommended):

- `nix profile install nixpkgs#z3 nixpkgs#boogie`

Alternatively, Z3 is usually available in system packages. For manual Boogie install instructions see subheading [Verifying the Boogie file](#verifying-the-boogie-file).

## Lifting a single binary

Many lifted examples are provided in the [tests directory][tests],
instructions are given [here](development/) as to how to add new tests.
The instructions below are for if you want to use the BASIL tool with new binary files.

BASIL takes as input:
- a .gts file, containing the ddisasm CFG and the ASLp semantics,
- a .relf file, containing the symbol table, and
- (optionally) a .spec file containing manually-written specifications
  (see [the secret\_write.spec][spec] for an example).

The steps below describe the dependencies required,
then the steps to obtain these input files
and run BASIL.

**See also:** [Quickly lifting a new example](quick-lifting.md)

[tests]: https://github.com/UQ-PAC/BASIL/tree/main/src/test/correct
[spec]: https://github.com/UQ-PAC/BASIL/blob/main/src/test/correct/secret_write/secret_write.spec

### Requirements

These instructions assume a Linux-GNU OS, BASIL itself will happily run on Windows,
and the lifting machinery is known to work under WSL.

- AArch64 cross-compilation toolchain (e.g. GCC or clang)
- AArch64 readelf (e.g. `aarch64-linux-gnu-readelf`)
- ddisasm, for disassembling binary files
- gtirb-semantics, to combine ddisasm's output with ASLp's instruction semantics
- BASIL itself

To install the AArch64 compilers and binutils, we suggest using your operating system's package manager.
On Ubuntu, the packages are _gcc-aarch64-linux-gnu_ and _binutils-aarch64-linux-gnu_.
For more complex examples, you may also need the AArch64 libc.

For ddisasm, gtirb-semantics and BASIL, we recommend installing these via our [pac-nix](https://github.com/katrinafyi/pac-nix) repository.
First, set up the Nix package manager according to the pac-nix instructions, then install the packages with:
```bash
nix profile install \
  github:katrinafyi/pac-nix#ddisasm \
  github:katrinafyi/pac-nix#gtirb-semantics \
  github:katrinafyi/pac-nix#basil
```

ddisasm may also be installed through its provided Docker images or
APT repository (as of Aug 2024, only supports Ubuntu 20.04).
You can also, of course, build any of these tools manually.

See also: [development: building](development/#building)
(primarily if you are interested in building BASIL manually).

### Preparation

1. Compile a C program into an Aarch64 binary, for example:

   ```bash
   aarch64-linux-gnu-gcc x.c
   ```

   You can double-check the produced file with:
   ```console
   $ file a.out
   a.out: ELF 64-bit LSB executable, ARM aarch64, version 1 (SYSV), dynamically linked, interpreter /lib/ld-linux-aarch64.so.1, BuildID[sha1]=ee0d9393526251c23d98f784b86fb3ad694e3517, for GNU/Linux 3.7.0, with debug_info, not stripped
   ```

2. Disassemble the binary with ddisasm.
   ```bash
   ddisasm a.out --ir a.gtirb
   ```

   This produces a GTIRB protobuffer file.
   You can inspect its contents, as JSON, with the proto-json.py tool
   (available in the gtirb-semantics repository and automatically installed by the gtirb-semantics Nix package).
   ```console
   $ proto-json.py a.gtirb --seek 8 | jq
         [...]

         "uuid": "j+onQhA3SSqXO8biANpjIw=="
       }
     ],
     "uuid": "INbyVMRqR9WT3OFbxYALhw==",
     "version": 4
   }
   ```

3. Add instruction semantics to the GTIRB file with gtirb-semantics (.gts means GTIRB with semantics).
   ```bash
   gtirb-semantics a.gtirb a.gts
   ```

   To display this output in a human-readable JSON format, use debug-gts.py
   (also installed alongside gtirb-semantics).
   This maps GTIRB basic blocks to source functions and displays instruction semantics
   alongside the assembly mnemonic.
   ```console
   $ debug-gts.py a.gts

   "cwoEFvsGRRm4Myu2pkmWFA==": {
     "name": "main [entry] [1/1]",
     "address": 4195972,
     "code": [
       {
         "mov w0, #100                        // =0x64": [
           "Stmt_Assign(LExpr_Array(LExpr_Var(\"_R\"),0),'0000000000000000000000000000000000000000000000000000000001100100')"
         ]
       },
       {
         "ret": [
           "Stmt_Assign(LExpr_Var(\"BTypeNext\"),'00')",
           "Stmt_Assign(LExpr_Var(\"__BranchTaken\"),Expr_Var(\"TRUE\"))",
           "Stmt_Assign(LExpr_Var(\"_PC\"),Expr_Array(Expr_Var(\"_R\"),30))"
         ]
       }
     ],
     "successors": {
       "RdkcpXXFTX6fJ6AY/CyB+w== / Unresolved ProxyBlock": "Edge.Label(type=Edge.Type.Return, conditional=False, direct=False, )"
     }
   },
   ```

4. Obtain the symbol table with readelf.
   ```
   aarch64-linux-gnu-readelf -s -r -W a.out > a.relf
   ```

### Using BASIL to generate Boogie

This generates a Boogie file in out.bpl.
```
basil --input a.gts --relf a.relf -o out.bpl
```
(If using BASIL compiled from source, substitute `./mill run` in place of `basil`.
Optionally add a specification file with `--spec`.)

### Verifying the Boogie file

Boogie requires .NET 6 (best installed through through your system packages).
Once .NET is available, Boogie can be installed with:
```bash
dotnet tool install --global Boogie
```
You may need to add ~/.dotnet/tools to your PATH.

Boogie can be run on the output .bpl file with the command
```bash
boogie /useArrayAxioms out.bpl
```

The `/useArrayAxioms` flag is necessary for Boogie versions 2.16.8 and greater;
for earlier versions it can be removed.
This improves the verification speed by using Boogie's axiomatic encoding of arrays rather than Z3's.
The axiomatic encoding is faster (1) because it does not support extensionality, and (2) because it sacrifices completeness.
This makes the most difference in SAT (non-verifying) cases.
We don't have reason to believe array extensionality is necessary for any of our proofs.

If using the built-in array theories, Z3's array extensionality reasoning can be disabled by
passing the `smt.array.extensional=false` configuration to Z3 through Boogie's CLI:

```
boogie example.bpl /proverOpt:O:smt.array.extensional=false
```

## Other useful tasks

To produce assembly from the binary, use:
```bash
aarch64-linux-gnu-objdump -d *.out
```

To view the hex dump of the data section of the binary:
```bash
readelf -x .data *.out
```

To examine the ASLp semantics of a particular instruction,
see [ASLp's README](https://github.com/UQ-PAC/aslp?tab=readme-ov-file#using-the-asl-partial-evaluator)
or [aslp-web](https://katrinafyi.github.io/aslp-web/).
