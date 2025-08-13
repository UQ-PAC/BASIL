#!/usr/bin/env bash
set -ue

if [[ -z "${GIT_ROOT:-}" ]] && command -v git &>/dev/null; then
  GIT_ROOT=$(git rev-parse --show-toplevel)
fi
: ${GIT_ROOT}
DIR=$(realpath --relative-to "$GIT_ROOT" .)

: ${DOCKER:=podman}
: ${DOCKER_PLATFORM:=--platform linux\/amd64}
: ${DOCKER_USER:=root}
: ${DOCKER_IMAGE:=ghcr.io/uq-pac/basil-tools-docker}

if [[ $# -lt 1 ]] || [[ "$1" == --help ]]; then
  echo "usage: $(basename $0) (pull | push | build | start | stop | shell | hash | env [--unset] | COMMAND...)"
  ! [[ $# -lt 1 ]]
  exit
fi

DOCKER_CMD="$(realpath $0)"

if [[ -z "${DOCKER_FLAKE:-}" ]] && [[ -r "$(dirname $DOCKER_CMD)/docker-flake.txt" ]]; then
  DOCKER_FLAKE="$(cat $(dirname $DOCKER_CMD)/docker-flake.txt)"
fi

: $DOCKER_FLAKE

# create unique names depending on the flake reference, to ensure the correct container
# is used.
# unique names depend only on DOCKER_FLAKE, allowing them to be computed without nix.
commit=$(printf '%s' "$DOCKER_FLAKE" | grep --only-matching -E '[0-9a-fA-F]{40}' | head -c8)
flake_hash=flake-$(printf '%s' "$DOCKER_FLAKE" | md5sum | cut -d' ' -f1 | head -c4)

if [[ -z "${DOCKER_TAG:-}" ]]; then
  DOCKER_TAG="$flake_hash-$commit"
fi

unique_image="$DOCKER_IMAGE:$DOCKER_TAG"
unique_container="container-$DOCKER_TAG"

# this allows the env subcommand to output syntax compatible with multiple shells
shell=$(basename $SHELL)
if [[ $shell == fish ]]; then
  unset='set --erase'
  unalias='functions --erase'
  eval='('
else
  unset=unset
  unalias=unalias
  eval='$('
fi


if [[ "$1" == pull ]]; then
  # pulls the unique image from the registry
  set -x
  exec $DOCKER pull $DOCKER_PLATFORM "$unique_image"

elif [[ "$1" == push ]]; then
  # pushes the unique image to the registry. image must already exist locally.
  set -x
  exec $DOCKER push "$unique_image"

elif [[ "$1" == build ]]; then
  # builds the docker image for running tools.
  # safe to re-run. if docker image is already up-to-date, should be reasonably fast.
  nix build "$DOCKER_FLAKE" --no-link
  nix build "$DOCKER_FLAKE.conf" --no-link
  conf=$(nix build "$DOCKER_FLAKE.conf" --no-link --print-out-paths)
  tag=$(nix eval --expr "with builtins; (fromJSON (unsafeDiscardStringContext (readFile $conf))).repo_tag" --impure --raw)
  if ! [[ "$tag" == "$DOCKER_IMAGE":* ]]; then
    printf '%s %s %s.\n' \
      "ERROR: docker image names do not match!" \
      "nix flake will build '$tag', but" \
      "DOCKER_IMAGE is '$DOCKER_IMAGE'" >&2
    exit 1
  fi
  set -x
  $(nix build "$DOCKER_FLAKE" --no-link --print-out-paths) | "$DOCKER" image load
  $DOCKER image tag "$tag" $unique_image
  exit

elif [[ "$1" == start ]]; then
  # starts an instance of the docker image.
  set -x
  exec $DOCKER run $DOCKER_PLATFORM -v"$GIT_ROOT:$GIT_ROOT" --rm -td --user $DOCKER_USER --name $unique_container $unique_image

elif [[ "$1" == stop ]]; then
  # stops the instance of the docker image.
  set -x
  exec $DOCKER stop -t 1 $unique_container
  # since --rm is given to `docker run`, this will also remove the container.

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

  # if --unset is used, removes all definitions
  isunset=$([[ $# -ge 2 ]] && [[ "$2" == --unset ]] && echo true || echo false)
  # if --reset is used, removes all definitions, then re-adds them based on defaults
  isreset=$([[ $# -ge 2 ]] && [[ "$2" == --reset ]] && echo true || echo false)

  if $isreset; then
    isunset=true
  fi

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
  echoexport DOCKER_IMAGE "$DOCKER_IMAGE"
  echoexport DOCKER_TAG "$DOCKER_TAG"
  echoexport DOCKER_PLATFORM "$DOCKER_PLATFORM"
  echoexport DOCKER "$DOCKER"
  echoexport DOCKER_USER "$DOCKER_USER"
  echoexport DOCKER_CMD "$DOCKER_CMD"
  echoexport GIT_ROOT "$GIT_ROOT"
  echo 'echo;'
  echoexport GCC "$DOCKER_CMD aarch64-unknown-linux-gnu-gcc"
  echoexport CLANG "$DOCKER_CMD aarch64-unknown-linux-gnu-clang"
  echoexport READELF "$DOCKER_CMD aarch64-unknown-linux-gnu-readelf"
  echoexport BAP "$DOCKER_CMD bap"
  echoexport DDISASM "$DOCKER_CMD ddisasm"
  echoexport PROTO_JSON "$DOCKER_CMD proto-json.py"
  # echoexport PROTO_JSON "/home/rina/progs/gtirb-semantics/scripts/proto-json.py"
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

  if $isreset; then
    echo "eval $eval$DOCKER_CMD env);"
  fi
  exit
fi

if [[ -n "${NIX_BUILD_TOP:-}" ]]; then
  set -x
  # if already inside a Nix shell, simply execute
  exec /usr/bin/_exec "$@"
else
  set -x
  # for other commands, execute within the container.
  exec $DOCKER exec --user $DOCKER_USER -w "$GIT_ROOT/$DIR" $unique_container /usr/bin/_exec "$@"
fi
