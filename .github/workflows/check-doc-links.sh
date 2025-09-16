#!/bin/bash

set -eu

dir="$(git rev-parse --show-toplevel)/out/allDocs.dest"

if ! [[ -d "$dir" ]]; then
  echo "allDocs dir '$dir' does not exist" >&2
  exit 2
fi

commit="${GITHUB_SHA:-$(git rev-parse HEAD)}"

if ! [[ -n "$commit" ]]; then
  echo "unable to detect git commit" >&2
  exit 3
fi

# last two --remap arguments will append index.html onto URLs with no extension (and allowing for # fragments)
# NOTE: this relies on --remap only actioning the /first/ matching regex
args=(
  "$dir"
  --fallback-extensions html,htm
  --root-dir "$dir"
  --remap "https://uq-pac\.github\.io/BASIL file://$dir/BASIL"
  --remap "https://github\.com/UQ-PAC/BASIL/blob/main https://github.com/UQ-PAC/BASIL/blob/$commit"
  --remap "https://github\.com/UQ-PAC/BASIL/tree/main https://github.com/UQ-PAC/BASIL/tree/$commit"
  --remap '(\.[a-z]+(#.*)?)$ $1'
  --remap '^file:///.* $0/index.html'
  --exclude '/scala-lang\.org/api/'
  --exclude '#L\d+$'
  --exclude 'https://hdl\.handle\.net/1911/96345'
  --insecure
  --no-progress
  --max-concurrency 16
  --max-retries 2
  --timeout 60
  "$@"
)

if "${LYCHEE_EXPORT_ARGS:-false}"; then
  printf '%q ' "${args[@]}" --format markdown
else
  exec lychee "${args[@]}" --cache
fi


