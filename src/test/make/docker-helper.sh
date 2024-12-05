#!/bin/bash -ue

root=$(git rev-parse --show-toplevel)
: ${GIT_ROOT:=${root}}

: ${DOCKER:=podman}
: ${DOCKER_USER:=root}
: ${DOCKER_IMAGE:=localhost/basil-tools-docker:latest}
: ${DOCKER_CONTAINER:=basil-build-container}

if [[ "$1" == build ]]; then
  # safe to re-run. if docker image is already up-to-date, should be reasonably fast.
  nix build "$DOCKER_FLAKE" --no-link
  $(nix build "$DOCKER_FLAKE" --no-link --print-out-paths) | "$DOCKER" image load
  exit
elif [[ "$1" == start ]]; then
  set -x
	exec $DOCKER run -v$GIT_ROOT:/build --rm -td --user $DOCKER_USER --name $DOCKER_CONTAINER $DOCKER_IMAGE
elif [[ "$1" == stop ]]; then
  set -x
	exec $DOCKER rm -f -t 1 $DOCKER_CONTAINER
elif [[ "$1" == hash ]]; then
  echo "$DOCKER_FLAKE"
  echo
  exec "$0" ls -1 /nix/store
fi

DIR=$(realpath --relative-to "$GIT_ROOT" .)
set -x
exec $DOCKER exec --user root -w "/build/$DIR" $DOCKER_CONTAINER /usr/bin/_exec "$@"
