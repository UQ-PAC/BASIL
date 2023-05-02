#!/bin/bash

# check if the input file was provided
if [ -z "$1" ]; then
  echo "Usage: $0 <input_file>"
  exit 1
fi

# check if the required libraries are installed
if ! command -v aarch64-linux-gnu-gcc &> /dev/null; then
    echo "Error: aarch64-linux-gnu-gcc is not installed"
    exit 1
fi

if ! command -v bap &> /dev/null; then
    echo "Error: bap is not installed"
    exit 1
fi

if ! command -v readelf &> /dev/null; then
    echo "Error: readelf is not installed"
    exit 1
fi
echo "Processing $1"
while true; do
  echo -n "."
  sleep 0.5
done &

# get the filename without the extension
filename=$(basename "$1")
filename="${filename%.*}"

# compile the C file into an executable
aarch64-linux-gnu-gcc -fno-plt -fPIC "$1" -o "${filename}.out"

# generate the ADT file using bap
bap --primus-lisp-semantics=disable "${filename}.out" -d adt:"${filename}.adt" -d bir:"${filename}.bir"

# generate the REFL file using readelf
readelf -s -r -W "${filename}.out" > "${filename}.relf"

# remove the executable
rm "${filename}.out"
kill $!
echo " Done"
