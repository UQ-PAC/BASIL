

.PHONY=lift
lift: free_low.adt free_low.bir free_low.relf

a.out: free_low.c 
	aarch64-linux-gnu-gcc -g -Wall -fno-pic -fno-stack-protector -Wextra free_low.c -O3
	bap-aslp a.out > free_low.bir

free_low.adt: a.out
	bap-aslp a.out -d adt:free_low.adt	

free_low.bir: a.out
	bap-aslp a.out -d > free_low.bir

free_low.relf: a.out
	aarch64-linux-gnu-readelf -s -r -W a.out > free_low.relf


