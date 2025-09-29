# Invoke this Makefile from the BASIL root directory with the command
#
# 	make -f [MAKEFILE_DIR]/Makefile
#
#   Optionally, you can set a compiler optimization level using the OPT variable:
#
#   make -f [MAKEFILE_DIR]/Makefile OPT=-O2

OPT ?= -O0
BASE_NAME=token_ring
ROOT_DIR:=$(shell dirname $(firstword $(MAKEFILE_LIST)))
NAME=$(ROOT_DIR)/$(BASE_NAME)
NAMEO=$(NAME).$(OPT)
SOURCES=$(shell find src/)
BASIL_OPTS=--simplify --dsa=  --dsa-split --dsa-checks --memory-transform --noif --assert-callee-saved never
COMPILER_OPTS=-march=armv8.1-a $(OPT) -D__BASIL__

all: $(NAMEO).relf $(NAMEO).gts $(NAMEO).bpl

$(NAMEO).bpl: $(NAMEO).gts $(NAMEO).relf $(SOURCES)
	./mill run --input $(NAMEO).gts --relf $(NAMEO).relf --output $@ $(BASIL_OPTS)

%.gts: %.gtirb
	gtirb-semantics $< $@

%.gtirb: %.out
	ddisasm $< --ir $@

%.relf: %.out
	greadelf -r -s -W $< > $@

%.$(OPT).out: %.c
	aarch64-unknown-linux-gnu-gcc $(COMPILER_OPTS) $< -o $@

clean:
	rm -f $(ROOT_DIR)/*.relf $(ROOT_DIR)/*.gts $(ROOT_DIR)/*.bpl $(NAME).ultimate.c

.PHONY: clean all
