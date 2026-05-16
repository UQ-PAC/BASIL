#!/bin/bash


ls ../tvbins/*/*.gtirb | xargs -I % -n 1 -P 8 bash scripts/run.sh %

python3 scripts/torow.py header
for i in output*;
do 
  cat $i | python3 scripts/torow.py $i
done

