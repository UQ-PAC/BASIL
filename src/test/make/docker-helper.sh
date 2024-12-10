#!/usr/bin/env bash
set -ue

if ! [[ -v GIT_ROOT ]] && command -v git &>/dev/null; then
  GIT_ROOT=$(git rev-parse --show-toplevel)
fi
: ${GIT_ROOT}
DIR=$(realpath --relative-to "$GIT_ROOT" .)

: ${DOCKER:=podman}
: ${DOCKER_USER:=root}
: ${DOCKER_IMAGE:=localhost/basil-tools-docker:latest}

if [[ $# -lt 1 ]] || [[ "$1" == --help ]]; then
  echo "usage: $(basename $0) (build | start | stop | hash | env [--unset] | COMMAND...)"
  ! [[ $# -lt 1 ]]
  exit
fi

DOCKER_CMD="$(realpath $0)"

if ! [[ -v DOCKER_FLAKE ]] && [[ -r "$(dirname $DOCKER_CMD)/docker-flake.txt" ]]; then
  DOCKER_FLAKE=$(cat $(dirname $DOCKER_CMD)/docker-flake.txt)
fi

: $DOCKER_FLAKE

# create unique names depending on the flake reference, to ensure the correct container
# is used.
flake_hash=$(printf '%s' "$DOCKER_FLAKE" | grep --only-matching -E '[0-9a-fA-F]{40}' | head -c8)
if [[ -z "$flake_hash" ]]; then
  flake_hash=md5-$(printf '%s' "$DOCKER_FLAKE" | md5sum | cut -d' ' -f1 | head -c4)
fi
unique_image=$DOCKER_IMAGE-$flake_hash
unique_container=container-$flake_hash

# this allows the env subcommand to output syntax compatible with multiple shells
shell=$(basename $SHELL)
if [[ $shell == fish ]]; then
  unset='set --erase'
  unalias='functions --erase'
else
  unset=unset
  unalias=unalias
fi


if [[ "$1" == build ]]; then
  # downloads/builds the docker image for running tools.
  # safe to re-run. if docker image is already up-to-date, should be reasonably fast.
  nix build "$DOCKER_FLAKE" --no-link
  set -x
  $(nix build "$DOCKER_FLAKE" --no-link --print-out-paths) | "$DOCKER" image load
  $DOCKER image tag $DOCKER_IMAGE $unique_image
  exit

elif [[ "$1" == start ]]; then
  # starts an instance of the docker image.
  set -x
	exec $DOCKER run -v"$GIT_ROOT:$GIT_ROOT" --rm -td --user $DOCKER_USER --name $unique_container $unique_image

elif [[ "$1" == stop ]]; then
  # starts the instance of the docker image.
  set -x
	exec $DOCKER rm -f -t 1 $unique_container

elif [[ "$1" == shell ]]; then
  # enters an interactive shell within the container.
  set -x
  exec $DOCKER exec -it --user $DOCKER_USER -w "$GIT_ROOT/$DIR" -eshell=1 $unique_container /usr/bin/_exec bash

elif [[ "$1" == hash ]]; then
  # outputs information about the docker image's version to stdout.
  echo "$DOCKER_FLAKE"
  echo
  exec "$DOCKER_CMD" bash -c 'ls -1 /nix/store | sort -k1.33' # sort /nix/store contents by name, not hash

elif [[ "$1" == env ]]; then
  # outputs commands to set the environment to stdout.
  # when passed to `eval`, these commands should prepare the shell for running
  # basil tests through docker.

  isunset=$([[ $# -ge 2 ]] && [[ "$2" == --unset ]] && echo true || echo false)

  function echoexport() {
    if $isunset; then
      echo echo "$unset" "$1" ';'
      echo "$unset" "$1" ';'
      return
    fi
    printf 'echo "%s = %s";\n' "$1" "$2"
    printf 'export %s="%s";\n' "$1" "$2"
  }

  echoexport USE_DOCKER "1"
  echoexport DOCKER_FLAKE "$DOCKER_FLAKE"
  echoexport DOCKER "$DOCKER"
  echoexport DOCKER_USER "$DOCKER_USER"
  echoexport DOCKER_IMAGE "$DOCKER_IMAGE"
  echoexport DOCKER_CMD "$DOCKER_CMD"
  echoexport GIT_ROOT "$GIT_ROOT"
  echo 'echo;'
	echoexport GCC "$DOCKER_CMD aarch64-unknown-linux-gnu-gcc"
	echoexport CLANG "$DOCKER_CMD aarch64-unknown-linux-gnu-clang"
	echoexport READELF "$DOCKER_CMD aarch64-unknown-linux-gnu-readelf"
	echoexport BAP "$DOCKER_CMD bap"
	echoexport DDISASM "$DOCKER_CMD ddisasm"
	# echoexport PROTO_JSON "$DOCKER_CMD proto-json.py"
	echoexport PROTO_JSON "/home/rina/progs/gtirb-semantics/scripts/proto-json.py"
	echoexport DEBUG_GTS "$DOCKER_CMD debug-gts.py"
	echoexport GTIRB_SEMANTICS "$DOCKER_CMD gtirb-semantics"
  echo 'echo;'
	if $isunset; then
	  echo "echo $unalias docker-helper.sh;"
	  echo "$unalias docker-helper.sh;"
	else
	  echo "echo alias docker-helper.sh = '$DOCKER_CMD';"
	  echo "alias 'docker-helper.sh=$DOCKER_CMD';"
	fi
	exit
fi

# for other commands, execute within the container.
if [[ -v NIX_BUILD_TOP ]]; then
  set -x
  exec /usr/bin/_exec "$@"
else
  set -x
  exec $DOCKER exec --user $DOCKER_USER -w "$GIT_ROOT/$DIR" $unique_container /usr/bin/_exec "$@"
fi
