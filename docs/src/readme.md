# BASIL (Boogie Analysis for Secure Information-Flow Logics)

[API Documentation](api/)

BASIL generates semantically equivalent Boogie source files (`.bpl`) from AArch64/ARM64
binaries that have been lifted intermediate formats. It takes as input the `.gts` format produced by
[gtirb-semantics](https://github.com/UQ-PAC/gtirb-semantics),  which consists of [ddisasm's](https://github.com/grammatech/ddisasm)
GTIRB  with [ASLp's](https://github.com/UQ-PAC/aslp) instruction semantics annotated as AuxData for each block.

Basil implements a concurrent information-flow logic verifier by encoding this logic in Boogie.
This logic is described [here](iflogic-encoding.pdf). Verifying examples can be found in
`src/test/correct` and non-verify examples can be found in `src/test/incorrect`.

### Basic Usage

The `./mill` script should be sufficient to bootstrap Scala, provided you have a JVM (>17) installed.
The test suite (and `--verify` flag) additionally require Boogie and Z3.

As input BASIL requires a `.gts` file produced by `gtirb-semantics`,
as well as a file containing the output of readelf (here denoted with `.relf`), both created from the same AArch64/ARM64 binary,
and outputs a semantically equivalent .bpl Boogie-language source file.

Detailed instructions for lifting examples and running BASIL can be found at [/usage](/BASIL/usage.html).

Basil also accepts BAP `.adt` files in place of the `.gts` file, however this feature is no longer actively maintained.

To build and run the tool use one of the following commands:

Linux / Mac OS:

```
./mill run --load-directory-gtirb ./example.gts [--output output.bpl] [--simplify] [--interpret]
```

Windows:

```
./mill.bat run --load-directory-gtirb ./example.gts [--output output.bpl] [--simplify] [--interpret]
```

Below is an overview of the BASIL project and its theoretical background.
To get started on development, see [development](development).


### Prior Art

Inasfar as BASIL is a program analysis tool written in Scala, that operates by translating programs to Boogie, there are also the analysis tools Viper and Vercors (based on Viper)
which use Boogie as a verification back-end.

- [Boogie](https://github.com/boogie-org/boogie) -- SMT-based Verifier backend used by Basil, developed Microsoft Research
- [Viper](http://viper.ethz.ch/tutorial/) -- Deductive verifier backend for separation logic from ETH Z
- [Vercors](https://github.com/utwente-fmt/vercors/wiki) -- Verifier based on Viper from U Twente
- [TIP](https://github.com/cs-au-dk/TIP/tree/master) static analysis tool basil's static analyses are based on. It
  is primarily an educational implementation, so is useful for understanding the algorithms but is not a performance baseline.
