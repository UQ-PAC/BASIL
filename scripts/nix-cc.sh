#!/bin/bash

prog="$0"

usage() {
  cat <<EOF
usage: $prog [--help] INPUTDIR OUTPUTDIR [COMMAND...]

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
EOF
  exit $1
}

die() {
  echo $2 >&2
  exit ${1:-1}
}

xrealpath() {
  # https://stackoverflow.com/a/3915420
  echo $(cd $(dirname $1); pwd)/$(basename $1)
}

xmktemp() {
  # https://ss64.com/mac/mktemp.html
  mktemp -t tmp.XXXXXXXXXX
}

command -v nix >/dev/null|| die 1 "nix is required"

if [[ "$@" == *'--help'* ]]; then
  usage 0
fi

if [[ "$#" -lt 2 ]]; then
  usage 1 >&2
fi

set -eu
set -o pipefail

inputdir="$(xrealpath $1)"
outputdir="$(xrealpath $2)"
shift
shift
command="${@:-gcc *.c}"

outfile=$(xmktemp)

nix-build --extra-experimental-features "nix-command flakes" --no-out-link - <<EOF > $outfile
  let
    pkgs = (builtins.getFlake "github:katrinafyi/pac-nix").lib.nixpkgs;
    aarch64pkgs = pkgs.pkgsCross.aarch64-multiplatform;
  in aarch64pkgs.runCommand "basil-test-files" {
      nativeBuildInputs = [
        aarch64pkgs.buildPackages.gcc
        aarch64pkgs.buildPackages.clang
        pkgs.ddisasm
        pkgs.gtirb-semantics
      ];
    } ''
      alias gcc=aarch64-unknown-linux-gnu-gcc
      alias clang=aarch64-unknown-linux-gnu-clang
      shopt -s expand_aliases
      mkdir \$out
      (set -x;
        cp -r \${$inputdir}/. .
        ls
        : BUILDING WITH COMMAND:
        $command
        : ... DONE BUILDING.
        aarch64-unknown-linux-gnu-readelf -s -r -W a.out > a.relf
        aarch64-unknown-linux-gnu-objdump -d a.out > a.objdump
        ddisasm a.out --ir a.gtirb
        gtirb-semantics a.gtirb a.gts
        proto-json.py --auxdata yes a.gts a.json
        cp -r a.out a.relf a.gtirb a.gts a.json a.objdump \$out
      )
    ''
EOF

nixout="$(cat $outfile)"
[[ -n "$nixout" ]] || die 2 "nix build did not print an output path?"

# copy files into given outputdir and add writable permissions.
# we cannot use --no-preserve=mode because macos doesn't have that.
mkdir -p $outputdir
cd $nixout

files=(*)
cp -rv "${files[@]}" $outputdir
cd $outputdir
chmod u+rw "${files[@]}"
