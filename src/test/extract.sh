
#!/bin/bash

set -xe
# Loopy benchmarks : https://github.com/microsoft/loop-invariant-gen-experiments

# wget https://github.com/microsoft/loop-invariant-gen-experiments/raw/refs/heads/main/dataset.zip


header=$(cat loopy_helper/unknown.h)

for i in $(unzip -l dataset.zip | grep 'dataset/.*$' -o | grep '\.c$'); 
do 
  
  tname=$(echo "$i" | sed  's/dataset\/\([-a-zA-Z0-9\/._]\+\).c/loopy_\1/' | sed 's/\//_/g')
  #rm -r analysis-regression/$tname
  mkdir -p analysis-regression/$tname
  fname=$(printf "analysis-regression/%s/%s.c" $tname $tname)
  cp loopy_helper/unknown.h $fname
  unzip -p dataset.zip $i | sed  's/extern int unknown_int(void);//' >> $fname
done;

#make DIRS=analysis-regression 
