#!/bin/bash

git show --oneline --no-patch
if bash -c 'pkill java  ; rm -rf out ; ./mill -i --ticker false --meta-level 1 compile' 2>&1 | grep 'Cyclic reference'; then
  echo has bug
  exit 0
else
  echo no bug
  exit 1
fi

