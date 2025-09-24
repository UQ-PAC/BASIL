#!/bin/bash

python3 scripts/torow.py header

for i in ../tvbins/*/*.gtirb
do
  java -jar out/assembly.dest/out.jar -i $i --lifter --pc assert --simplify-tv boop --simplify-tv-dryrun #| python3 scripts/torow.py $i
done

