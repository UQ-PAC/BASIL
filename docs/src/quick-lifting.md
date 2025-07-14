# Quickly lifting a new example

This page describes [`nix-cc.sh`](https://github.com/UQ-PAC/BASIL/blob/main/scripts/nix-cc.sh),
a quick method to compile and lift a new example for use with the Basil toolchain.
This script automates the steps described in [usage.md](usage.md) and automatically
fetches the required tools from the [pac-nix](https://github.com/katrinafyi/pac-nix/)
repository.

`nix-cc.sh` requires a directory of C source file(s), and it produce:
- a cross-compiled aarch64-linux ELF binary,
- a readelf output,
- an objdump output,
- a GTIRB and gtirb-semantics output, and
- a JSON-formatted version of the gtirb-semantics output.

This includes all the files needed for Basil (namely, the gtirb-semantics and readelf outputs),
along with some extra files which might be useful for inspecting the compiled program.

Compared to other methods, nix-cc.sh is
- self-contained and isolated in a sandbox, which should ensure consistency across platforms,
- correctly configured with the aarch64 C libraries, and
- it runs natively on your computer, so it should be faster than emulation or virtual machines.

## Introduction

The script lives inside the scripts/ folder of the Basil repository.
It is a Bash script which should work on both Linux and Mac OS.

First, make sure that pac-nix is set up. This can be done by following
[the "first time" instructions for pac-nix](https://github.com/katrinafyi/pac-nix?tab=readme-ov-file#first-time).
Make sure to follow all the numbered steps, including checking that the cache is working.

Then, to use the script, simply pass a directory of C files and an output directory:
```bash
scripts/nix-cc.sh src/test/correct/arrays_simple /tmp/basil-out
```
The first time you run the script, it will take some time to fetch the cross-compilers
for aarch64-linux. Subsequent runs will be much faster. This will also use about 5 GB of disk space.

Once the script finishes, you can point Basil at the output directory using `--load-directory-gtirb`.
```bash
./mill run --load-directory-gtirb /tmp/basil-out/a.gts
```
Be sure to include the a.gts file, otherwise the files will not be found correctly.

## Command-line arguments

```
usage: scripts/nix-cc.sh [--help] INPUTDIR OUTPUTDIR [COMMAND...]

compiles C programs to aarch64-linux binaries and lifts them
using tools provided by the pac-nix repository.

this will create the files a.out a.relf a.gtirb a.gts a.json a.objdump
in the OUTPUTDIR.

positional arguments:
  INPUTDIR     input directory (this will be copied! make sure it is not too big)
  OUTPUTDIR    output directory to place binary and lifted files
  COMMAND      custom compiler command and arguments. this should produce an
                   'a.out' file (available compilers: gcc, clang)
                   (default: gcc with all C files in directory)
```

## Example output

Here is an excerpt of the output:
```bash
fetching github input 'github:katrinafyi/pac-nix'
this derivation will be built:
  /nix/store/eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee-basil-test-files.drv
building '/nix/store/eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee-basil-test-files.drv'...
+++ cp -r /nix/store/eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee-arrays_simple/. .
+++ ls
arrays_simple.c  clang  config.mk  env-vars
+++ : BUILDING WITH COMMAND:
+++ aarch64-unknown-linux-gnu-gcc arrays_simple.c
+++ : ... DONE BUILDING.
+++ aarch64-unknown-linux-gnu-readelf -s -r -W a.out
+++ aarch64-unknown-linux-gnu-objdump -d a.out
+++ ddisasm a.out --ir a.gtirb
Building the initial gtirb representation [   0ms]
Processing module: a.out
    disassembly              load [   1ms]    compute [   5ms]  transform [   0ms]
    SCC analysis                              compute [   0ms]  transform [   0ms]
    no return analysis       load [   0ms]    compute [   0ms]  transform [   0ms]
    function inference       load [   0ms]    compute [   0ms]  transform [   0ms]
+++ gtirb-semantics a.gtirb a.gts
Lifting
Successfully lifted 107 instructions in 2.776012 sec (2.774692 user time) (0 failure: 0 unique opcodes) (0.00 0000 cache hit rate)
+++ proto-json.py --auxdata yes a.gts a.json
+++ cp -r a.out a.relf a.gtirb a.gts a.json a.objdump /nix/store/eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee-basil-test-files
'a.gtirb' -> '/tmp/basil-out/a.gtirb'
'a.gts' -> '/tmp/basil-out/a.gts'
'a.json' -> '/tmp/basil-out/a.json'
'a.objdump' -> '/tmp/basil-out/a.objdump'
'a.out' -> '/tmp/basil-out/a.out'
'a.relf' -> '/tmp/basil-out/a.relf'
```

## Important notes

- The entire input directory will be copied, so it's best to make it small - only containing
  the files relevant to the example you want.
  If you pass a directory with too many files, this will consume more disk space.
- The script will produce output files with non-specific names (such as a.out).
  Be aware of this when choosing the output directory.
- Built binaries will live in the Nix store, taking up disk space. To remove old files,
  you can use `nix-store --gc`.
- Nix does caching of builds. If you run the script multiple times without changing the
  input directory, it may skip the bulid entirely. The (cached) output will still be copied
  into the output directory.

