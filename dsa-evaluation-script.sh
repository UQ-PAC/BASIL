#!/bin/bash

# Create/overwrite the results file
results_file="dsa-results.csv"
echo "Program,Node Count,Nodes Collapsed,Max Cell Density,Time(ms)" > $results_file

# List of test files
tests=(
  "eq.gts"
  "split.gts"
  "stack.gts"
  "complete_callgraph_1.gts"
  "complete_callgraph_2.gts"
  "complete_callgraph_3.gts"
  "complete_callgraph_4.gts"
)

cntlm="cntlm-noduk_O0.gts"

# Run both commands for each test
for test in "${tests[@]}"; do
  echo "Running $test with default dsa..."
  ./mill run --input "dsa-testing/${test}" --simplify --dsa= --append-dsa-stats $results_file
done
for test in "${tests[@]}"; do
  echo "Running $test with --dsa-split --dsa-eqv..."
  ./mill run --input "dsa-testing/${test}" --simplify --dsa= --dsa-split --dsa-eqv --append-dsa-stats $results_file
done

# Test CNTLM
echo "Running $cntlm with default dsa..."
./mill run --input "dsa-testing/${cntlm}" --simplify --dsa=
echo "Running $cntlm with --dsa-split..."
./mill run --input "dsa-testing/${cntlm}" --simplify --dsa= --dsa-split
