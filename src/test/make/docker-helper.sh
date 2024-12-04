#!/bin/bash -xu

: "$@" $GIT_ROOT

DIR=$(realpath --relative-to "$GIT_ROOT" .)
exec docker exec --user root -it -w "/build/$DIR" $DOCKER_CONTAINER /usr/bin/_exec "$@"
