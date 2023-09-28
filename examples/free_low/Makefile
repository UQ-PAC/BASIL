

.PHONY=lift
lift: free-low-bad free-low-good

free-low-good: free_low.c 
	aarch64-linux-gnu-gcc -O3 -g -Wall -fno-pic -fno-stack-protector -Wextra free_low.c -o free-low-good 
	bap-aslp free-low-good -d adt:free-low-good.adt	
	bap-aslp free-low-good > free-low-good.bir
	aarch64-linux-gnu-readelf -s -r -W free-low-good > free-low-good.relf

free-low-bad: free_low.c 
	aarch64-linux-gnu-gcc -g -Wall -fno-pic -fno-stack-protector -Wextra free_low.c  -o free-low-bad
	bap-aslp free-low-bad -d adt:free-low-bad.adt	
	bap-aslp free-low-bad > free-low-bad.bir
	aarch64-linux-gnu-readelf -s -r -W free-low-bad > free-low-bad.relf
