
.EXPORT_ALL_VARIABLES:
NAME=$(shell basename $(PWD))

CC=aarch64-linux-gnu-gcc
#CFLAGS=-fno-pic -fno-plt
TARGET=aarch64-linux-gnu

BAP=bap-aslp
READELF=aarch64-linux-gnu-readelf
BASIL=$(realpath ../../../../target/scala-3.3.1/wptool-boogie-assembly-0.0.1.jar)

C_SOURCE=$(realpath $(wildcard *.c))
SPEC=$(realpath $(wildcard *.spec))
EXTRA_SPEC=$(realpath $(wildcard *.bpl))
BASIL_FLAGS=--boogie-use-lambda-stores
BOOGIE_FLAGS=/proverOpt:O:smt.array.extensional=false

LIFT_ARTEFACTS=$(NAME).adt $(NAME).bir $(NAME).relf

SUBDIRS := $(wildcard */.)

TARGETS := all verify
.PHONY : $(TARGETS) $(SUBDIRS)

$(TARGETS): $(SUBDIRS)

$(SUBDIRS):
	-$(MAKE) -C $@ $(MAKECMDGOALS)

