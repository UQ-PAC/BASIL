#!/usr/bin/bash
set -xe

MILL_CMD=mill 
$MILL_CMD mill.bsp.BSP/install 
fixed=$(perl -pe 's/"argv":\[".*?",/"argv":["$MILL_CMD",/g' .bsp/mill-bsp.json) 
echo $fixed > .bsp/mill-bsp.json 

