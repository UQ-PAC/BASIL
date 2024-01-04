.EXPORT_ALL_VARIABLES:
NAME=$(shell basename $(PWD))
GIT_ROOT?=$(shell git rev-parse --show-toplevel)

GCC ?= aarch64-linux-gnu-gcc
CLANG ?= clang
CC ?= $(GCC)
#CFLAGS=-fno-pic -fno-plt
TARGET=aarch64-linux-gnu

BAP?=bap
READELF ?= aarch64-linux-gnu-readelf
BASIL=$(GIT_ROOT)/target/scala-3.3.1/wptool-boogie-assembly-0.0.1.jar

C_SOURCE ?=$(realpath $(wildcard *.c))
SPEC ?=$(realpath $(wildcard *.spec))
EXTRA_SPEC ?=$(realpath $(wildcard *.bpl))
BASIL_FLAGS=--boogie-use-lambda-stores
#BOOGIE_FLAGS=/proverOpt:O:smt.array.extensional=false
BOOGIE_FLAGS ?= /useArrayAxioms

LIFT_ARTEFACTS=$(NAME).adt $(NAME).bir $(NAME).relf

ENABLED_COMPILERS ?= clang clang_O2 clang_pic clang_no_plt_no_pic gcc gcc_O2 gcc_no_plt_no_pic gcc_pic

TARGETS := all verify clean cleanall cleanlift
.PHONY : $(TARGETS) $(ENABLED_COMPILERS)

$(TARGETS): $(ENABLED_COMPILERS)

$(ENABLED_COMPILERS):
	mkdir -p $@/
	-$(MAKE) -C $@/ -f $(GIT_ROOT)/src/test/make/$@.mk $(MAKECMDGOALS)

