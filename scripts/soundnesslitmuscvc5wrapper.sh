#!/bin/bash

timelimit=3000

fname=$1
echo $fname
in_query=$(grep -E '(source[0-9]+|tgt[0-9]+)' -o $fname | wc -l)
cvcresult=$(cvc5 --dump-unsat-cores $fname --tlimit $timelimit)
in_unsatcore=$(echo $cvcresult | grep -E '(source[0-9]+|tgt[0-9]+)' -o | wc -l)
exitcode=$?
if [[ exitcode ]] then
  printf "%d%% contributed  \t%d/%d asserts\n" $(echo "scale=0; 100 * $in_unsatcore / $in_query" | bc) $in_unsatcore $in_query
else 
  echo "timeout"
fi

echo ""


