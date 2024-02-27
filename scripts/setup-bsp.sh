#!/usr/bin/bash
set -xe

./mill mill.bsp.BSP/install 
fixed=$(perl -pe 's/"argv":\[".*?",/"argv":[".\/mill",/g'  .bsp/mill-bsp.json) 
echo $fixed > .bsp/mill-bsp.json 

