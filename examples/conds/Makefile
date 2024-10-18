

.PHONY=all
all: conds.out conds.adt conds.relf

conds.out : conds.c
	aarch64-unknown-linux-gnu-gcc conds.c -o conds.out 

conds.adt : conds.out
	bap conds.out -d adt:conds.adt

conds.relf : conds.out
	readelf -s -r -W conds.out > conds.relf


