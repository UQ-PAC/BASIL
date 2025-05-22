#!/bin/bash -u

set -o pipefail

# Executes the Mill Scalatest runner (https://www.scalatest.org/user_guide/using_the_runner),
# while collating its output. When testForkGrouping is used, the output would usually be
# interspersed due to the multiple concurrent processes (as opposed to threads).
#
# This script will re-group each process's output, making use of the "[001]" prefix which Mill
# inserts on each line.

out=$(mktemp)
red=$(tput setaf 1)

set -x

./mill test.compile

./mill --ticker true -j3 test "$@" &> $out
code=$?

sort --key=1,1 --stable $out
echo
grep -F "$red" $out

exit $code
