#!/bin/bash

# Create/overwrite the results file
echo "Program,Node Count,Nodes Collapsed,Max Cell Density,Time(ms)" > dsa-results.csv

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

# Run both commands for each test
for test in "${tests[@]}"; do
  echo "Running $test with default dsa..."
  ./mill run --input "dsa-testing/${test}" --simplify --dsa=
done
for test in "${tests[@]}"; do
  echo "Running $test with --dsa-split --dsa-eqv..."
  ./mill run --input "dsa-testing/${test}" --simplify --dsa= --dsa-split --dsa-eqv
done

echo "Done! Results in dsa-results.csv"