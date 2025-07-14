#!/bin/bash

set -eu

dir="$(git rev-parse --show-toplevel)"
mode="${1:-run}"

# last two --remap arguments will append index.html onto URLs with no extension (and allowing for # fragments)
# NOTE: this relies on --remap only actioning the /first/ matching regex
args=(
  "$dir"
  --fallback-extensions html,htm
  --root-dir "$dir"
  --remap "https://uq-pac.github.io/BASIL file://$dir"
  --remap '(\.[a-z]+(#.*)?)$ $1'
  --remap '^file:///.* $0/index.html'
  --exclude '/scala-lang\.org/api/'
  --exclude '#L\d+$'
  --no-progress
  --format markdown
)

if [[ "$mode" == export ]]; then
  printf '%q ' "${args[@]}"
else
  lychee "${args[@]}"
fi


