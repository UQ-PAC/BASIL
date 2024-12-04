#!/bin/bash -ue

root=$(git rev-parse --show-toplevel)
: ${GIT_ROOT:=${root}}

: ${DOCKER:=docker}
: ${DOCKER_IMAGE:=localhost/basil-tools-docker:latest}
: ${DOCKER_CONTAINER:=basil-build-container}

FLAKE=github:katrinafyi/pac-nix/cceaf2eabacb584ef66c738f5e057f3fa9cbbd84#basil-tools-docker

if [[ "$1" == build ]]; then
  # safe to re-run. if docker image is already up-to-date, should be reasonably fast.
  nix build "$FLAKE" --no-link
  $(nix build "$FLAKE" --no-link --print-out-paths) | "$DOCKER" image load
  exit
elif [[ "$1" == start ]]; then
  set -x
	exec $DOCKER run -v$GIT_ROOT:/build --rm -td --user root --name $DOCKER_CONTAINER $DOCKER_IMAGE
elif [[ "$1" == stop ]]; then
  set -x
	exec $DOCKER rm -f -t 1 $DOCKER_CONTAINER
elif [[ "$1" == hash ]]; then
  set -x
  exec "$0" ls -1 /nix/store
fi

DIR=$(realpath --relative-to "$GIT_ROOT" .)
set -x
exec $DOCKER exec --user root -it -w "/build/$DIR" $DOCKER_CONTAINER /usr/bin/_exec "$@"
