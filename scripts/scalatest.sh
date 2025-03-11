#!/bin/bash

# Executes the full Scalatest runner (https://www.scalatest.org/user_guide/using_the_runner).
#
# This is needed to use some more advanced arguments of the runner, since `./mill test.run`
# silently ignores some arguments (for example, -n and -l).

# NOTE: executing the runner through this script may try to start a GUI.
#       to avoid this and use the console, pass -o.

classes="$(./mill show test.compile | grep '"classes"' | cut -d'"' -f4 | cut -d: -f4)"

if ! [[ -d "$classes" ]]; then
  echo "unable to determine mill class output directory: $classes" >&2
  exit 1
fi

exec ./mill test.runMain org.scalatest.tools.Runner -R "$classes" "$@"

