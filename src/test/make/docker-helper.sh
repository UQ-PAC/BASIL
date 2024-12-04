#!/bin/bash -ue

root=$(git rev-parse --show-toplevel)
: ${GIT_ROOT:=${root}}

: ${DOCKER:=docker}
: ${DOCKER_IMAGE:=localhost/basil-tools-docker:latest}
: ${DOCKER_CONTAINER:=basil-build-container}

if [[ "$1" == start ]]; then
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
