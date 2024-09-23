#!/bin/bash

# check if the inputs are provided
if [[ -z "$1" || -z "$2" || -z "$3" ]]; then
  echo "Usage: $0 <.c file> <-gcc or -clang> <-bap or -gtirb>"
  exit 1
fi

if [ "$2" == "-gcc" ]; then
  if ! command -v aarch64-linux-gnu-gcc &> /dev/null; then
    echo "Error: aarch64-linux-gnu-gcc is not installed"
    exit 1
  fi
  gcc=true
elif [ "$2" == "-clang" ]; then
  if ! command -v clang-15 &> /dev/null; then
    echo "Error: clang-15 is not installed"
    exit 1
  fi
  gcc=false
else
  echo "Usage: $0 <.c file> <-gcc or -clang> <-bap or -gtirb>"
  exit 1
fi

if [ "$3" == "-bap" ]; then
  if ! command -v bap &> /dev/null; then
    echo "Error: bap is not installed"
    exit 1
  fi
  bap=true
elif [ "$3" == "-gtirb" ]; then
  if ! command -v ddisasm &> /dev/null; then
    echo "Error: ddisasm is not installed"
    exit 1
  fi
  if ! command -v gtirb-semantics &> /dev/null; then
    echo "Error: gtirb-semantics is not installed"
    exit 1
  fi
  if ! command -v debug-gts.py &> /dev/null; then
    echo "Error: debug-gts.py is not installed"
    exit 1
  fi
  bap=false
else
  echo "Usage: $0 <.c file> <-gcc or -clang> <-bap or -gtirb>"
  exit 1
fi

if ! command -v aarch64-linux-gnu-readelf &> /dev/null; then
    echo "Error: aarch64-linux-gnu-readelf is not installed"
    exit 1
fi

# get the filename without the extension
filename=$(basename "$1")
filename="${filename%.*}"

# compile the C file into an executable

if [ "$gcc" = true ]; then 
  aarch64-linux-gnu-gcc "$1" -o "${filename}.out"
else
  clang-15 -target aarch64-linux-gnu "$1" -o "${filename}.out"
fi

if [ "$bap" = true ]; then 
  bap "${filename}.out" -d adt:"${filename}.adt" -d bir:"${filename}.bir"
else
  ddisasm "${filename}.out" --ir "${filename}.gtirb"
  gtirb-semantics "${filename}.gtirb" "${filename}.gts"
  rm -rf "${filename}.gtirb"
  debug-gts.py "${filename}.gts" > "${filename}.json"
fi

aarch64-linux-gnu-readelf -s -r -W "${filename}.out" > "${filename}.relf"

rm "${filename}.out"