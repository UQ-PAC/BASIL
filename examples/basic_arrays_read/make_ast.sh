#!/bin/bash
ORIGINAL_DIR=$(pwd)
# Search for C file in the current directory
CFILE=$(find . -maxdepth 1 -name "*.c" -type f -print -quit)
# Extract the file name without extension
FILENAME=$(basename "$CFILE" ".c")
GTSARGS="$ORIGINAL_DIR/$FILENAME.gtirb ../asl-interpreter/prelude.asl ../mra_tools ../asl-interpreter $ORIGINAL_DIR/$FILENAME.gts"
BASLIARGS="compile; run $FILENAME.gts $FILENAME.ast.json.t; exit"
rm  $FILENAME $FILENAME.ast $FILENAME.ast.json $FILENAME.gtirb $FILENAME.gts
# Compile and disassemble the C file
aarch64-linux-gnu-gcc "$CFILE" -o "$FILENAME"
ddisasm "$FILENAME" --ir "$FILENAME.gtirb"
# Continue with the rest of the script
cd /home/wnedo/GITRB/gtirb-semantics
dune exec gtirb_semantics  $GTSARGS > "$ORIGINAL_DIR/$FILENAME.ast"
cp "$ORIGINAL_DIR/$FILENAME.gts" /home/wnedo/GITRB/basli
cd /home/wnedo/GITRB/basli
echo
echo $BASLIARGS | sbt
mv $FILENAME.ast.json.t "$ORIGINAL_DIR"
cd $ORIGINAL_DIR
python3 -mjson.tool "$FILENAME.ast.json.t" > "$FILENAME.ast.json"
rm "$FILENAME.ast.json.t"
cat "$FILENAME.ast.json"
