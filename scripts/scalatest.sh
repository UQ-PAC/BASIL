#!/bin/bash -u

set -o pipefail

# Executes the Mill Scalatest runner (https://www.scalatest.org/user_guide/using_the_runner),
# running tests in parallel collating their output.
#
# When tests are run in parallel, the output would usually be interspersed due to the
# multiple concurrent processes. This script will re-group each process's output, making
# use of the "[001]" prefix which Mill inserts on each line.
#
# NOTE: this script require GNU coreutils, so it may not work on Mac.

out=$(mktemp)
red=$(TERM=xterm tput setaf 1)

[[ -n "$red" ]] || exit 1

set -x

export BASIL_TEST_PARALLEL=true
./mill test.compile

./mill --ticker true test "$@" &> $out
code=$?

sort --key=1,1 --stable $out
echo
grep -F "$red" $out

exit $code
