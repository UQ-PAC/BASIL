#!/usr/bin/env bash


for i in $(find src/test/correct/ -iname '*.expected'); do
  cp "$i" "$i.bpl";
done

for i in $(find src/test/incorrect/ -iname '*.expected'); do
  cp "$i" "$i.bpl";
done

# need this exact version to avoid bugs
# opam pin git://github.com/sneeuwballen/benchpress#b0033cb1f900161f9e3212b47242899f59827895

benchpress run -j12 -c  scripts/benchpressconf.sexp --task run-comparison --progress --csv=bench.csv


