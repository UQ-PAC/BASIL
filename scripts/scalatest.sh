#!/bin/bash

# Executes the full Scalatest runner (https://www.scalatest.org/user_guide/using_the_runner).
#
# This is needed to use some more advanced arguments of the runner, since `./mill test.run`
# silently ignores some arguments (for example, -n and -l).

# NOTE: executing the runner through this script may try to start a GUI.
#       to avoid this and use the console, pass -o.

# useful commands
# ---------------
#
# - to include only a specific tag, use -n.
# - to exclude a specific tag, use -l.
# - these can be combined. for example, to select all StandardSystemTest except Slow ones:
#
#     ./scripts/scalatest.sh -o -n test_util.tags.StandardSystemTest -l test_util.tags.Slow
#
# - when passing tags through the command line, the fully-qualified name must be used.
# - to print individual test durations, pass D to -o. for example, -oD.
# - to print a summary of failing tests at the end of the output, pass I to -o.
#

classes="$(./mill -i --ticker false show test.compile | grep '"classes"' | cut -d'"' -f4 | cut -d: -f4)"

if ! [[ -d "$classes" ]]; then
  echo "unable to determine mill class output directory: $classes" >&2
  exit 1
fi

exec ./mill -i --ticker false test.runMain org.scalatest.tools.Runner -R "$classes" "$@"

