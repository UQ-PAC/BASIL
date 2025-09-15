#!/bin/bash -eu

set -o pipefail

test_dir=src/test

echo '::group::All test suites:'
tests="$(./mill -i test.testOnly -- -t '' -oW | grep ':$' | tr -d ':' | cut -d' ' -f2 | sort)"
echo "$tests"
echo '::endgroup::'
echo
echo '::group::Disabled test suites:'
grep --color=always '@test_util\.tags\.DisabledTest' --context=1 -R $test_dir
echo '::endgroup::'
echo

# Locates the definition of the given test case, and prints the class definition
# with two preceding lines of context.
find_test_case() {
  t="$1"
  shift
  # NOTE: \> matches the end of a word
  if ! grep 'class\s\+'"$t"'\>' --before-context=2 -R $test_dir "$@"; then
    echo "find_test_case failed to find '$t'" >&2
    return 1
  fi
}

ok=true
echo '::group::Test suites with no tag annotations:'
for t in $tests; do
  defn="$(find_test_case "$t")"

  # search for lines which are entirely "@test_util.tags.*Test*"
  # leading - is produced by grep to mark prefixes
  if ! grep -q -- '-@test_util\.tags\..\+Test.*$' <<< "$defn"; then
    echo 'test suite' "'$t'" 'has no `@test_util.tags.*Test*` annotation:' >&2
    find_test_case "$t" --color=always || true
    echo
    ok=false
  fi
done
if $ok; then
  echo '(none - this is good!)'
fi
echo '::endgroup::'

$ok

