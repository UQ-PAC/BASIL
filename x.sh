#!/bin/bash

shopt -s nullglob
for f in $(find src/test/dsa -name '*.c'); do
  if [[ "$f" = src/test/dsa/correct/split/split.c ]]; then
    f=src/test/dsa/correct/split/clang/split
  fi

  srcs=(`eval echo $(dirname $f)/'*.{gts,adt}'`)
  relf=`eval echo $(dirname $f)/'*.relf'`
  if [[ -f "$relf" ]]; then
    ./mill runMain DsaDot -- ${srcs[0]} $relf | grep digraph --after-context=100000 > $(basename $f).dsa.dot
  fi
done
