#!/bin/bash -eu

set -o pipefail

test_dir=src/test

echo '::group::All test suites:'
tests="$(./mill test.testOnly -- -t '' -oW | tr -d ':' | sort)"
echo "$tests"
echo '::endgroup::'
echo
echo '::group::Disabled test suites:'
grep '@test_util.tags.DisabledTest' --context=1 -R $test_dir
echo '::endgroup::'
echo

ok=true
echo '::group::Test suites with no tag annotations:'
for t in $tests; do
  defn="$(grep 'class\s\+'"$t"'[ (]' --before-context=2 -R $test_dir)"
  if ! grep -q '@test_util.tags.' <<< "$defn"; then
    echo 'test suite has no `@test_util.tags.*` annotation:' >&2
    grep 'class\s\+'"$t"'[ (]' -R $test_dir
    echo
    ok=false
  fi
done
echo '::endgroup::'

$ok

