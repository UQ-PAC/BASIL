NAME=sync_read

all: relf gts

binary:
	clang-15 -target aarch64-linux-gnu -march=armv8.1-a -std=c11 $(NAME).c -o $(NAME).out

relf: binary
	aarch64-linux-gnu-readelf -s -r -W $(NAME).out > $(NAME).relf

gts: binary
	ddisasm $(NAME).out --ir $(NAME).gtirb
	gtirb-semantics $(NAME).gtirb $(NAME).gts
	rm -rf $(NAME).gtirb